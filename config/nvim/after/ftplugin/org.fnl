(local {: buf-keymaps!} (require :utils))

(let [{: action} (require :orgmode)]
  (buf-keymaps! {:i {:<C-CR> {:desc "Org meta return"
                               :callback #(action :org_mappings.meta_return)}}}))

(vim.cmd.abbreviate ["<buffer>" "--" "- [ ]"])
