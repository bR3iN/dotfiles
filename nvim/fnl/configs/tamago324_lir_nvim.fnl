(local {: add!} (require :pkg))

(local actions (require :lir.actions))
(local mark_actions (require :lir.mark.actions))
(local clipboard_actions (require :lir.clipboard.actions))

(add! "nvim-lua/plenary.nvim")
; (add! "kyazdani42/nvim-web-devicons")

(let [{: setup} (require :lir)]
  (setup {
          :show_hidden_files false
          :mappings {"<C-]>" actions.edit
                     "<C-s>" actions.split
                     "<C-v>" actions.vsplit
                     "q" actions.quit
                     "-" actions.up
                     "." actions.toggle_show_hidden
                     "R" actions.rename
                     "N" actions.newfile
                     "K" actions.mkdir
                     "D" actions.delete}
          :hide_cursor true}))
