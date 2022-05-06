(let [{: set_default_keymaps : setup} (require :leap)]
    (set_default_keymaps)
    (setup {:safe_labels {}}))
