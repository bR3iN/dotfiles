(local {: put!} (require :utils))
(local {: spawn-capture-output} (require :utils.async))
(local lspconfig (require :lspconfig))

(local configs [{:name :clangd
                 :config {}
                 :keymaps {[:n :<C-c>] #(vim.cmd.ClangdSwitchSourceHeader)}}
                ;; {:name :bashls}
                ;; {:name :vimls}
                {:name :fennel_ls}
                {:name :purescriptls
                 :config {:settings {:purescript {:formatter :purs-tidy
                                                  :codegenTargets [:corefn]
                                                  :addSpagoSources true}}}}
                {:name :basedpyright}
                {:name :cmake}
                ;; {:name :rust_analyzer}  ; Configured by rustaceans instead
                {:name :hls}
                ;; {:name :marksman}
                {:name :racket_langserver}])

(local default-keymaps
       {; [:n :gD] vim.lsp.buf.declaration
        ; [:n :gt] vim.lsp.buf.type_definition
        ; [:n :gd] vim.lsp.buf.definition
        ; [:n :K] vim.lsp.buf.hover
        ; [:n :gi] vim.lsp.buf.implementation
        ; [:n :<C-k>] vim.lsp.buf.signature_help
        ; [:n :<leader>rn] vim.lsp.buf.rename
        ; [:n :<leader>ca] vim.lsp.buf.code_action
        ; [:n :gr] vim.lsp.buf.references
        ; [:n :gqq] vim.lsp.buf.format
        ; [:v :gq] vim.lsp.buf.format
        [:n :<leader>od] vim.diagnostic.open_float
        [:n "[d"] vim.diagnostic.goto_prev
        [:n "]d"] vim.diagnostic.goto_next})

(local mk-on_attach
       (fn [?extra-keymaps]
         (let [keymaps (vim.tbl_extend :force default-keymaps
                                       (or ?extra-keymaps {}))]
           (fn [_client bufnr]
             ;; Always show signcolumn for that
             ;; buffer, so the code doesn't move
             ;; horizontally when the number of
             ;; diagnostics changes to or from zero.
             ;; TODO: We assume here that the code is
             ;; always opened in the current window,
             ;; not sure how true this.
             (tset vim.wo 0 :signcolumn :yes)
             ;; Create local keymaps
             (let [opts {:noremap true :silent true :buffer bufnr}]
               (each [[mode lhs] rhs (pairs keymaps)]
                 (vim.keymap.set mode lhs rhs opts)))))))

(local mk-capabilities
       #(let [(ok mod) (pcall require :cmp_nvim_lsp)]
          (if ok (mod.default_capabilities)
              (vim.lsp.protocol.make_client_capabilitites))))

(each [_ {: name : ?config : ?keymaps} (ipairs configs)]
  (let [capabilities (mk-capabilities)
        on_attach (mk-on_attach ?keymaps)
        config (vim.tbl_extend :error {: capabilities : on_attach}
                               (or ?config {}))
        {: setup} (. lspconfig name)]
    (setup config)))

(let [border :rounded
      cap-to-handler {:textDocument/hover vim.lsp.handlers.hover
                      :textDocument/signatureHelp vim.lsp.handlers.signature_help}]
  (vim.diagnostic.config {:float {: border}})
  (tset (require :lspconfig.ui.windows) :default_options {: border})
  (each [cap handler (pairs cap-to-handler)]
    (tset vim.lsp.handlers cap (vim.lsp.with handler {: border}))))

(let [zk (require :zk)
      util (require :zk.util)
      create-and-insert-link #(let [loc (util.get_lsp_location_from_caret)
                                    title (vim.fn.input "Title: ")]
                                (if (not= (length title) 0)
                                    (zk.new {: title
                                             :edit false
                                             :insertLinkAtLocation loc})))
      ; create-note #(let [title (vim.fn.input "Title: ")]
      ;                (if (not= (# title) 0)
      ;                  (zk.new {: title})))
      ; TODO reactivate the above or document snippet
      create-note #(zk.new)
      extra-keymaps {[:i :<C-h>] :<Esc>hcT|
                     [:i :<C-l>] :<Esc>2la
                     [:i :<C-y>] "<Esc>2hvT|uf]2la"
                     [:n :<localleader>nz] create-note
                     [:n :<localleader>no] #(zk.edit)
                     [:n :<localleader>nb] #(vim.cmd.ZkBacklinks)
                     [:i :<C-i>] "<C-o>:ZkInsertLink<CR>"
                     [:i :<C-j>] create-and-insert-link
                     [:i :<C-p>] #(spawn-capture-output :zk-screenshot nil
                                                        (fn [code
                                                             _signal
                                                             stdout
                                                             _stderr]
                                                          (if (= 0 code)
                                                              (put! (.. "![["
                                                                        stdout
                                                                        "]]")))))}]
  (zk.setup {:picker :telescope
             :lsp {:config {:on_attach (mk-on_attach extra-keymaps)
                            :capabilities (mk-capabilities)}}}))
