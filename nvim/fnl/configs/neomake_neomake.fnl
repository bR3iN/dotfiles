(local {: augroup! : concat} (require :utils.nvim))
(import-macros {: let!} :utils.macros)

(let! neomake_error_sign
  {:text "➤"
   ; :texthl "NeomakeErrorSign"
   })
(let! neomake_warning_sign
  {:text "➤"
   ; :texthl "NeomakeWarningSign"
   })
(let! neomake_message_sign
  {:text "➤"
   ; :texthl "NeomakemessageSign"
   })
(let! neomake_info_sign
  {:text "➤"
   ; :texthl "NeomakeInfoSign"
   })

(let [join-cmds #(table.concat $1 " | ")
      cmd (join-cmds
            ["hi link NeomakeErrorSign          DiagnosticSignError"
             "hi link NeomakeWarningSign        DiagnosticSignWarn"
             "hi link NeomakeMessageSign        DiagnosticSignHint"
             "hi link NeomakeInfoSign           DiagnosticSignInfo"
             "hi link NeomakeVirtualtextError   DiagnosticError"
             "hi link NeomakeVirtualtextWarning DiagnosticWarn"
             "hi link NeomakeVirtualtextMessage DiagnosticHint"
             "hi link NeomakeVirtualtextInfo    DiagnosticInfo"])
      autocmd! (augroup! "neomake_highlighting")]
  (autocmd! :ColorScheme "*" cmd))

