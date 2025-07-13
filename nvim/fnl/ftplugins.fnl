(import-macros {: setl! : setl+ : setl-} :utils.macros)

(local {: nmap!
        : vmap!
        : tmap!
        : cmap!
        : imap!
        : xmap!
        : smap!
        : omap!
        : command!
        : augroup!
        : autocmd!
        : put!} (require :utils))

{:fennel (fn []
           (nmap! [:buffer] :gqq (.. ":<C-u>w<CR>:" "! fnlfmt --fix %<CR><CR>"))
           (xmap! [:buffer] :gq (.. ":'<,'>! fnlfmt -<CR>"))
           (let [{: find_files} (require :telescope.builtin)
                 {: cache-prefix} (require :hotpot.api.cache)]
             (setl- iskeyword ".") ; Search in cache
             (nmap! [:buffer :silent] :<leader>fc
                    #(find_files {:cwd (cache-prefix) :hidden true}))))
 :c #(setl! shiftwidth 2)
 :cpp #(setl! shiftwidth 2)
 :dap-repl #(vim.cmd "abbreviate <buffer> e -exec")
 :markdown (fn []
             (setl! nobackup)
             (setl! nowritebackup)
             (setl+ iskeyword "\\")
             (vim.cmd "abbreviate <buffer> \\bf \\mathbf
                                                  abbreviate <buffer> \\rm \\mathrm
                                                  abbreviate <buffer> \\cal \\mathcal
                                                  abbreviate <buffer> \\bb \\mathbb
                                                  abbreviate <buffer> \\frak \\mathfrak
                                                  abbreviate <buffer> iff if and only if"))
 :org (fn []
        (let [{: action} (require :orgmode)]
          (imap! [:buffer] :<C-CR> #(action :org_mappings.meta_return)))
        (vim.cmd "abbreviate -- - [ ]"))
 :qf #(nmap! [:buffer] :q :<Cmd>cclose<CR>)
 :rust (fn []
         (nmap! [:buffer] :<leader>cr ":<C-u>Crun<CR>")
         (nmap! [:buffer] :<leader>cb ":<C-u>make build<CR>")
         (nmap! [:buffer] :<leader>ct ":<C-u>make test<CR>")
         (nmap! [:buffer] :<leader>cl ":<C-u>make clippy<CR>")
         (nmap! [:buffer] :<leader>rf ":<C-u>RustFmt<CR>")
         (vmap! [:buffer] :<leader>rf ":RustfmtRange<CR>"))
 :sh #(setl! shiftwidth 4)
 :zsh #(setl! shiftwidth 4)}
