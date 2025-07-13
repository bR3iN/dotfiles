(local M {})

(fn M.starts-with [str prefix]
  (= (string.sub str 1 (length prefix)) prefix))

(fn M.remove-prefix [str prefix]
  (string.sub str (+ 1 (length prefix))))

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

(fn parse-maps-tbl [tbl]
  (let [out []]
    (fn step [prefix tbl]
      (each [key val (pairs tbl)]
        ;; `key` can be a list of keys
        (let [keys (if (M.table? key) key [key])]
          (each [_ key (ipairs keys)]
            (let [new-prefix (.. prefix key)]
              (if (M.table? val)
                  ;; Recurse on nested table
                  (step new-prefix val)
                  (set (. out new-prefix) val)))))))

    (step "" tbl)
    out))

(fn keymaps-ext! [tbl ?opts]
  (let [default-opts {:silent true}
        opts (if ?opts (vim.tbl_extend :force default-opts ?opts) default-opts)]
    (each [modes maps-tbl (pairs tbl)]
      (each [lhs rhs (pairs (parse-maps-tbl maps-tbl))]
        (vim.keymap.set modes lhs rhs opts)))))

(fn M.keymaps! [tbl]
  (keymaps-ext! tbl))

(fn M.local-keymaps! [tbl]
  (keymaps-ext! tbl {:buffer true}))

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

; (fn M.augroup! [name ?clear]
;   (let [id (let [clear (or ?clear true)]
;              (vim.api.nvim_create_augroup name {: clear}))]
;     (fn [event pattern callback ?opt-tbl]
;       (let [set-cb (fn [tbl]
;                      (case (type callback)
;                        :string (set tbl.command callback)
;                        :function (set tbl.callback callback)))
;             opt-tbl (doto (or ?opt-tbl {})
;                       (tset :group id)
;                       (tset :pattern pattern)
;                       (set-cb))]
;         (vim.api.nvim_create_autocmd event opt-tbl)))))

(fn M.autocmd! [& args]
  (let [inner (fn [name clear cmds]
                (let [group (vim.api.nvim_create_augroup name {: clear})]
                  (each [_ {: event & opt_tbl} (ipairs cmds)]
                    (vim.api.nvim_create_autocmd event
                                                 (vim.tbl_extend :error opt_tbl
                                                                 {: group})))))]
    (case args
      [name cmds] (inner name true cmds)
      [name clear cmds] (inner name clear cmds)
      _ (error "bad arguments"))))

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

(var augroup nil)

(fn M.init []
  (set augroup (vim.api.nvim_create_augroup :init {:clear true})))

(fn init-aucmds [tbl]
  (each [event opts (pairs tbl)]
    (let [group (or augroup (error "not initialized"))
          opts (vim.tbl_extend :error opts {: group})]
      (vim.api.nvim_create_autocmd event opts))))

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
         : map
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
                    (init-aucmds {:ColorScheme {:callback set-hls!}})))
                (when setup
                  (each [module opts (pairs (eval setup))]
                    (use-setup module opts)))
                (when autocmds
                  (init-aucmds (eval autocmds)))
                (when reload
                  (each [_ mod (ipairs (eval reload))]
                    (M.reload mod)))
                (when config
                  (config))
                (when command
                  (each [name spec (pairs (eval command))]
                    (use-command name spec)))
                (when map
                  (M.keymaps! (eval map))))]
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

M
