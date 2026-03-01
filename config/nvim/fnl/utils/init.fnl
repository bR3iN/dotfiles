(local M {})

(fn _G.unload [mod]
  (set (. _G.package.loaded mod) nil))

;; FIXME: vim.vim.startswith
(fn M.starts-with [str prefix]
  (= (string.sub str 1 (length prefix)) prefix))

(fn M.remove-prefix [str prefix]
  (string.sub str (+ 1 (length prefix))))

(fn M.remove-opt-prefix [str prefix]
  (if (M.starts-with str prefix)
      (M.remove-prefix str prefix)
      str))

(fn M.contains [str substr]
  (not= (string.find str substr) nil))

; ; Exports `nil?`, `function?`, etc.
; (let [mk-is-type (fn [tp]
;                    (fn [el]
;                      (= (type el)
;                         tp)))]
;   (each [_ tp (ipairs [:nil :function :string :table :number])]
;     (let [name (.. tp :?)]
;       (tset M name (mk-is-type tp)))))

(fn M.nil? [el] (= (type el) :nil))
(fn M.function? [el] (= (type el) :function))
(fn M.string? [el] (= (type el) :string))
(fn M.table? [el] (= (type el) :table))
(fn M.number? [el] (= (type el) :number))

(fn M.empty? [str]
  (= (length str) 0))

(fn list? [el]
  ;; FIXME: Check if robust enough
  (and (M.table? el) (. el 1)))

(fn into-list [val]
  (if (list? val) val [val]))

(fn M.reload [mod]
  (set (. _G.package.loaded mod) nil)
  (require mod))

(fn parse-keys [keys]
  (vim.api.nvim_replace_termcodes keys true false true))

(fn M.feed! [keys ?remap]
  (vim.api.nvim_feedkeys (parse-keys keys) (if ?remap :m :n) false))

(var augroup nil)

(fn M.init []
  (set augroup (vim.api.nvim_create_augroup :my_init {:clear true})))

(fn init-autocmd [event opts]
  (let [group (or augroup (error "not initialized"))
        opts (vim.tbl_extend :error opts {: group})]
    ;; Prevent autocmd from being removed when returning last expression...
    (when opts.callback
      (let [cb opts.callback]
        (set opts.callback #(do
                              (cb $...)
                              nil))))
    (vim.api.nvim_create_autocmd event opts)))

(fn parse-maps-tbl [tbl]
  (let [out []]
    (fn step [curr-prefix tbl]
      ;; Recurse `tbl`, collecting found mappings in `out`.
      (each [key val (pairs tbl)]
        ;; `key` can be a list of keys
        (let [keys (if (M.table? key) key [key])]
          (each [_ key (ipairs keys)]
            (let [new-prefix (.. curr-prefix key)]
              ;; `val` can be either a "submap" table which we recurse on, a table-valued leaf (i.e. keymap), which we identify by the presence of a :callback key, a string-valued leaf, or `false` to delete an existing keybind.
              (if (M.table? val)
                  (let [{: callback & rest} val]
                    (if callback
                        ;; Identify table-valued leaves by the presence of :callback
                        (set (. out new-prefix) {:rhs callback :opts rest})
                        ;; Otherwise, they are nested keymaps and we recurse
                        (step new-prefix rest)))
                  (set (. out new-prefix) {:rhs val :opts {}})))))))

    (step "" tbl)
    out))

(fn pop! [tbl key]
  (let [val (. tbl key)]
    ;; Always clear, including explicit `false` values; normalize to nil
    (set (. tbl key) nil)
    (or val nil)))

(fn auto-remap? [rhs]
  (and (M.string? rhs)
       (let [rhs (string.lower rhs)]
         (or (M.contains rhs :<plug>) (M.contains rhs :<localleader>)
             (M.contains rhs :<leader>)))))

(fn build-rhs [lhs rhs {: repeatable : deprecated : remap}]
  (fn []
    (when deprecated
      (vim.notify (string.format "Don't use %s: %s" lhs deprecated)
                  vim.log.levels.WARN))
    ;; The actual rhs
    (if (M.string? rhs) (M.feed! rhs remap) (rhs))
    ;; Integrate with vim-repeat
    (when repeatable
      (if _G.vim.fn.repeat#set
          (_G.vim.fn.repeat#set (parse-keys lhs))
          (vim.notify_once (.. "can't enable dot-repeat for '" lhs "'")
                           vim.log.levels.WARN)))))

(fn resolve-map [lhs rhs opts]
  (if rhs
      (let [opts (vim.tbl_extend :force {:silent true :remap false} opts)
            opts (if (auto-remap? rhs) (doto opts (tset :remap true)) opts)
            rhs (build-rhs lhs rhs
                           {:remap opts.remap
                            :repeatable (pop! opts :repeatable)
                            :deprecated (pop! opts :deprecated)})]
        (values lhs rhs opts))
      (values lhs rhs opts)))

(fn apply! [{: modes : lhs : rhs : opts}]
  (if rhs (vim.keymap.set modes lhs rhs opts)
      (vim.keymap.del modes lhs {:buffer opts.buffer})))

(fn map-for-buffer [buffer map]
  (vim.tbl_extend :force map
                  {:opts (vim.tbl_extend :error map.opts {: buffer})}))

(fn ft-apply! [map fts]
  (let [callback (fn [{: buf}]
                   (apply! (map-for-buffer buf map)))]
    (init-autocmd :FileType {:pattern fts : callback})))

(fn lsp-apply! [map lsps ?ft]
  (let [;; Normalize
        lsps (if (M.string? lsps) {lsps {}}
                 (list? lsps) (collect [_ name (ipairs lsps)]
                                (values name {}))
                 lsps)
        ?fts (-?> ?ft (into-list))]
    (each [name opts (pairs lsps)]
      (let [buf-ok? (fn [buf client_id]
                      (and (-> client_id
                               (vim.lsp.get_client_by_id)
                               (?. :name)
                               (= name))
                           ;; If specified, ensure filetype is correct
                           (or (not ?fts)
                               (let [ft (. vim.bo buf :filetype)]
                                 (vim.list_contains ?fts ft)))))
            callback (fn [{: buf :data {: client_id}}]
                       (when (buf-ok? buf client_id)
                         (apply! (map-for-buffer buf map))))]
        (init-autocmd (vim.tbl_extend :error opts
                                      {:event :LspAttach : callback}))))))

(fn M.keymaps! [tbl]
  (let [base-opts (or (pop! tbl :opts) {})]
    (each [modes maps-tbl (pairs tbl)]
      (each [lhs {: rhs : opts} (pairs (parse-maps-tbl maps-tbl))]
        (let [opts (vim.tbl_extend :force base-opts opts)
              (lhs rhs opts) (resolve-map lhs rhs opts)
              lsp (pop! opts :lsp)
              ft (pop! opts :ft)
              map {: lhs : rhs : opts : modes}]
          (if lsp (lsp-apply! map lsp ft)
              ft (ft-apply! map ft)
              (apply! map)))))))

(fn M.ft! [tbl]
  (each [filetype callback (pairs tbl)]
    (init-autocmd :FileType {:pattern filetype : callback})))

(fn parse-opt-name [name]
  "Parse option name to extract base name and operation suffix (+, -, ^)"
  (let [last-char (string.sub name -1)]
    (case last-char
      "+" {:name (string.sub name 1 -2) :op :append}
      "-" {:name (string.sub name 1 -2) :op :remove}
      "^" {:name (string.sub name 1 -2) :op :prepend}
      _ {:name name :op :set})))

(fn opts-inner! [tbl opt-obj]
  (each [name val (pairs tbl)]
    (let [{:name opt-name :op op} (parse-opt-name name)
          opt-meta (. opt-obj opt-name)]
      (case op
        :set (let [current (opt-meta:get)]
               (when (not= current val)
                 (tset opt-obj opt-name val)))
        _ (: opt-meta op val)))))

(fn M.opts! [tbl]
  (opts-inner! tbl vim.opt))

(fn M.buf-opts! [tbl]
  (opts-inner! tbl vim.opt_local))

(fn M.command! [lhs rhs ?opt-tbl]
  (let [opt-tbl (or ?opt-tbl {})]
    (vim.api.nvim_create_user_command lhs rhs opt-tbl)))

(fn M.autocmd! [& cmds]
  (each [_ {: event & opts} (ipairs (into-list cmds))]
    (init-autocmd event opts)))

(fn M.replace-termcodes [keys]
  (vim.api.nvim_replace_termcodes keys true false true))

(fn M.put! [text]
  (vim.api.nvim_put [text] "" false true))

(fn M.hl! [name {: extend & opts}]
  (let [opts (if extend
                 ;; If `extend` is a string, extend this hl group, otherwise use this one
                 (let [name (if (= true extend) name extend)]
                   (->> {: name}
                        (vim.api.nvim_get_hl 0)
                        (vim.tbl_extend :keep opts)))
                 opts)]
    (vim.api.nvim_set_hl 0 name opts)))

(fn M.hls! [tbl]
  (each [name opts (pairs tbl)]
    (M.hl! name opts)))

(fn has-schema? [str]
  (case (string.find str "://")
    nil false
    _ true))

(fn expand-src [src]
  (if (not (has-schema? src))
      (.. "https://github.com/" src)
      src))

(fn expand-spec [spec]
  (let [spec (if (M.string? spec) {:src spec} spec)]
    (set spec.src (expand-src spec.src))
    spec))

(var pkgs-pending [])

(fn eval [obj]
  (if (M.function? obj) (obj) obj))

(fn use-command [name spec]
  (let [create vim.api.nvim_create_user_command]
    (if (M.table? spec)
        (let [{: callback & opts} spec]
          (create name callback opts))
        (create name spec {}))))

(fn use-setup [module opts]
  (let [m (require module)
        opts (if (M.function? opts) (opts m) opts)]
    (m.setup opts)))

(fn M.use! [spec ?opts]
  (let [specs (if (vim.islist spec) spec [spec])
        specs (vim.tbl_map expand-spec specs)
        {: setup
         : config
         : sync
         : keymaps
         : autocmds
         : init
         : reload
         : hl
         : ft
         : signs
         : command} (or ?opts {})
        reload (if (M.table? reload) reload [reload])
        set-hls! (when hl
                   #(each [name opts (pairs (eval hl))]
                      (M.hl! name opts)))
        before (fn []
                 (when init
                   (init)))
        after (fn []
                ;; Some plugins (e.g. indent-blankline) require highlight groups to be set before setup.
                (when set-hls!
                  (set-hls!)
                  (init-autocmd :ColorScheme {:callback set-hls!}))
                (when setup
                  (each [module opts (pairs (eval setup))]
                    (use-setup module opts)))
                (when autocmds
                  (each [event opts (pairs (eval autocmds))]
                    (init-autocmd event opts)))
                (when reload
                  (each [_ mod (ipairs (eval reload))]
                    (M.reload mod)))
                (when config
                  (config))
                (when set-hls!
                  (set-hls!))
                (when signs
                  (M.signs! (eval signs)))
                (when ft
                  (M.ft! (eval ft)))
                (when command
                  (each [name spec (pairs (eval command))]
                    (use-command name spec)))
                (when keymaps
                  (M.keymaps! (eval keymaps))))]
    (if sync
        (do
          (before)
          (vim.pack.add specs)
          (after))
        (table.insert pkgs-pending {: specs : before : after}))))

(fn M.sync []
  (let [pkgs pkgs-pending]
    (set pkgs-pending [])
    (each [_ {: before} (pairs pkgs)]
      (before))
    (-> pkgs
        (vim.iter)
        (: :map #(. $1 :specs))
        (: :flatten)
        (: :totable)
        (vim.pack.add))
    (each [_ {: after} (pairs pkgs)]
      (after))))

;; FIXME: Explicitly splitting config & enabling allows the latter to be machine local
(fn M.lsps! [tbl]
  (each [name config (pairs tbl)]
    ;; FIXME: backwards compatible but can opt-out
    (let [enable (or (pop! config :enable) true)]
      (vim.lsp.config name config)
      (when enable
        (vim.lsp.enable name)))))

(fn M.dispatchables! [mode actions]
  (let [var-tbl (. vim mode)
        val (or (. var-tbl :my-dispatchables) {})]
    (each [id action (pairs actions)]
      (set (. val id) action))
    (set (. var-tbl :my-dispatchables) val)))

(fn M.dispatch! [id ...]
  ((or (?. vim.b :my-dispatchables id) (?. vim.g :my-dispatchables id)
       (error (.. "no dispatchable configured with id " id))) ...))

;; Loads module and calls its `setup` function
(fn M.setup [mod ?opts]
  (let [{: setup} (require mod)]
    (setup (or ?opts {}))))

(macro inc! [pos]
  `(set ,pos (+ 1 ,pos)))

;; (fn insert-nested [tbl arr]
;;   (inc! (. tbl :/))
;;   (-> arr
;;       (vim.iter)
;;       (: :fold tbl (fn [tbl comp]
;;                      (let [inner (. tbl comp)]
;;                        (if (not= inner nil)
;;                          (do 
;;                            ;; Using the "/" key (which can't appear as a path component) to store the count
;;                            (inc! (. inner :/))
;;                            inner)
;;                          ;; Create new entry
;;                          (let [inner {:/ 1}]
;;                            (set (. tbl comp) inner)
;;                            inner)))))))

;; Get an iterator over the reverse path components of `path`
(fn rev-comps [path]
  (-> path
      (vim.split "/")
      (vim.iter)
      (: :rev)
      (: :filter #(< 0 (length $1)))))

;; Nested-insert of the reverse path components of `path` into `tbl`.
(fn index-insert [index path]
  (inc! (. index :/))
  (-> path
      (vim.split "/")
      (vim.iter)
      (: :rev)
      (: :filter #(< 0 (length $1)))
      (: :fold index (fn [tbl comp]
                       (let [inner (. tbl comp)]
                         (if (not= inner nil)
                             (do
                               ;; Using the "/" key (which can't appear as a path component) to store the count
                               (inc! (. inner :/))
                               inner)
                             ;; Create new entry
                             (let [inner {:/ 1}]
                               (set (. tbl comp) inner)
                               inner)))))
      ;; (: :totable)
      ))

;; Creates index of all buffer paths components in reverse order
(fn create-bufname-index []
  (-> (vim.api.nvim_list_bufs)
      (vim.iter)
      (: :filter #(. vim.bo $1 :buflisted))
      (: :filter #(= (. vim.bo $1 :buftype) ""))
      (: :filter vim.api.nvim_buf_is_loaded)
      (: :map vim.api.nvim_buf_get_name)
      (: :fold {:/ 0} (fn [acc path]
                        (index-insert acc path)
                        acc))))

;; (fn unique-from-here? [sub-index]
;;   (= (. sub-index :/) 1))

(fn unique-path [path index]
  (local comps [])
  (var sub-index index)
  ;; TODO: confirm do-while semantics of &until
  ;; Always process at least one component
  (each [comp (rev-comps path) ;; Abort if either not in index anymore or unique from here
         &until (and (< 0 (length comps))
                     (or (not sub-index) (= (. sub-index :/) 1)))]
    (table.insert comps comp)
    (set sub-index (?. sub-index comp)))
  (-> comps
      (vim.iter)
      (: :rev)
      (: :join "/")))

;; (vim.print (create-bufname-index))
;; (vim.print (unique-path (vim.api.nvim_buf_get_name 0) (create-bufname-index)))

(fn M.unique-bufname [name]
  (unique-path name (create-bufname-index)))

(fn M.get-cwd-override []
  ;; If in an oil buffer, use the buffer's directory as `cwd`
  (if (= vim.o.filetype :oil)
      (let [{: get_current_dir} (require :oil)]
        (get_current_dir))
      nil))

(fn M.signs! [tbl]
  (each [key value (pairs tbl)]
    (vim.fn.sign_define key value)))

(fn M.pack_sync []
  (let [plugins (vim.pack.get nil {:info false})
        to-delete (icollect [_ {: active :spec {: name}} (ipairs plugins)]
                    (when (not active) name))]
    (when (not= 0 (length to-delete))
      (vim.pack.del to-delete))))

M
