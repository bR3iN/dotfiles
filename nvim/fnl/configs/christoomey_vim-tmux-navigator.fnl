(import-macros {: let!} :utils.macros)
(local {: nmap! : tmap!} (require :utils.nvim))

(let! tmux_navigator_no_mappings 1)

(local direction-to-keys
  {:Left  [:h :left]
   :Right [:l :right]
   :Up    [:k :up]
   :Down  [:j :down]})

(fn setup [gen-lhs]
  (each [direction keys (pairs direction-to-keys)]
    (each [_ key (ipairs keys)]
      (nmap! (gen-lhs key) (.. ":<C-u>TmuxNavigate" direction "<CR>"))
      (tmap! (gen-lhs key) (.. "<C-\\><C-n>:TmuxNavigate" direction "<CR>")))))

{ : setup }
