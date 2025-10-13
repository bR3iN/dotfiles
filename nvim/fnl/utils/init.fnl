(local M {})

(fn _G.unload [mod]
  (set (. _G.package.loaded mod) nil))

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

(fn M.reload [mod]
  (set (. _G.package.loaded mod) nil)
  (require mod))

(var augroup nil)

(fn M.init []
  (set augroup (vim.api.nvim_create_augroup :my_init {:clear true})))

;; FIXME
(fn init-aucmd [event opts]
  (let [group (or augroup (error "not initialized"))
        opts (vim.tbl_extend :error opts {: group})]
    ;; Prevent autocmd from being removed when returning last expression...
    (when opts.callback
      (let [cb opts.callback]
        (set opts.callback #(do
                              (cb $...)
                              nil))))
    (vim.api.nvim_create_autocmd event opts)))

(fn init-aucmd! [tbl]
  (each [event opts (pairs tbl)]
    (init-aucmd event opts)))

(set M.UNBIND :unset-key)

(fn parse-maps-tbl [tbl]
  (let [out []]
    (fn step [curr-prefix tbl]
      ;; Recurse `tbl`, collecting found mappings in `out`.
      (each [key val (pairs tbl)]
        ;; `key` can be a list of keys
        (let [keys (if (M.table? key) key [key])]
          (each [_ key (ipairs keys)]
            (let [new-prefix (.. curr-prefix key)]
              ;; `val` can be either a "submap" table which we recurse on, a table-valued leaf (i.e. keymap), which we identify by the presence of a :callback key, a string-valued leaf, or an `M.UNBIND`-valued leaf that deletes an existing keybind.
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

(fn keymaps-inner! [tbl ?base-opts]
  (let [default-opts {:silent true}]
    (each [modes maps-tbl (pairs tbl)]
      (each [lhs {: rhs : opts} (pairs (parse-maps-tbl maps-tbl))]
        (let [opts (vim.tbl_extend :force default-opts (or ?base-opts {}) opts)]
          (if (= rhs M.UNBIND)
              (vim.keymap.del modes lhs)
              (vim.keymap.set modes lhs rhs opts)))))))

(fn M.ft! [tbl]
  (each [filetype callback (pairs tbl)]
    (init-aucmd! {:FileType {:pattern filetype : callback}})))

;; TODO: Unnecessary?
;; (fn M.ft-keymaps! [tbl]
;;   (M.ft-autocmd! (vim.tbl_map (fn [maps]
;;                                 #(keymaps-inner! maps {:buffer true}))
;;                               tbl)))

(fn M.keymaps! [tbl ?opts]
  (keymaps-inner! tbl ?opts))

(fn M.local-keymaps! [tbl]
  (keymaps-inner! tbl {:buffer true}))

(fn parse-map-options [list]
  (local opt-tbl {:noremap true :silent true}) ; Default options
  (each [_ opt (ipairs list)]
    (case opt
      :remap (set opt-tbl.noremap false)
      :verbose (set opt-tbl.silent false)
      _ (tset opt-tbl opt true)))
  opt-tbl)

; Wrapper around `nvim_buf_set_keymap` that accepts options as a list. The
; `remap` option will be automatically added if `rhs` contains `<Plug>` or `<plug>`.
(fn M.set-keymap [mode lhs rhs ?opt-ls]
  (let [opt-tbl (parse-map-options (or ?opt-ls []))]
    (if (and (M.string? rhs)
             (or (M.contains rhs :<Plug>) (M.contains rhs :<plug>)))
        (set opt-tbl.noremap false))
    (vim.keymap.set mode lhs rhs opt-tbl)))

; Define the `<mode>map!` functions, e.g. `nmap!`; they can be called as
; either `(nmap! lhs rhs)` or `(nmap! [:opt1 :opt2] lhs rhs)`
(each [_ mode (ipairs [:n :v :x :s :o :i :l :c :t])]
  (tset M (.. mode :map!)
        #(if (M.nil? $3)
             (M.set-keymap mode $1 $2)
             (M.set-keymap mode $2 $3 $1))))

(fn M.command! [lhs rhs ?opt-tbl]
  (let [opt-tbl (or ?opt-tbl {})]
    (vim.api.nvim_create_user_command lhs rhs opt-tbl)))

(fn M.autocmd! [& cmds]
  (each [_ {: event & opts} (ipairs cmds)]
    (init-aucmd event opts)))

(fn M.replace-termcodes [keys]
  (vim.api.nvim_replace_termcodes keys true false true))

(fn M.put! [text]
  (vim.api.nvim_put [text] "" false true))

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
         : command} (or ?opts {})
        reload (if (M.table? reload) reload [reload])
        before (fn []
                 (when init
                   (init)))
        after (fn []
                ;; Some plugins (e.g. indent-blankline) require highlight groups to be set before setup.
                (when hl
                  (let [set-hls! #(each [name opts (pairs (eval hl))]
                                    (M.hl! name opts))]
                    (set-hls!)
                    (init-aucmd! {:ColorScheme {:callback set-hls!}})))
                (when setup
                  (each [module opts (pairs (eval setup))]
                    (use-setup module opts)))
                (when autocmds
                  (init-aucmd! (eval autocmds)))
                (when reload
                  (each [_ mod (ipairs (eval reload))]
                    (M.reload mod)))
                (when config
                  (config))
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

(fn M.hl! [name {: extend & opts}]
  (let [opts (if extend
                 (->> {: name}
                      (vim.api.nvim_get_hl 0)
                      (vim.tbl_extend :keep opts))
                 opts)]
    (vim.api.nvim_set_hl 0 name opts)))

(fn M.lsps! [tbl]
  (each [name config (pairs tbl)]
    (vim.lsp.config name config)
    (when (not= name "*")
      (vim.lsp.enable name))))

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
         &until (and
                  (< 0 (length comps))
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

M
