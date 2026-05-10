(vim.api.nvim_buf_create_user_command 0 "Hex"
                                      (fn []
                                        (vim.cmd "%!xxd")
                                        ;; Set ft=xxd, saving the old one
                                        (set vim.b._old_ft vim.bo.filetype)
                                        (set vim.bo.filetype :xxd))
                                      {})

(vim.api.nvim_buf_create_user_command 0 "UnHex"
                                      (fn []
                                        (vim.cmd "%!xxd -r")
                                        ;; Restore old ft (if applicable)
                                        (when vim.b._old_ft
                                          (set vim.bo.filetype vim.b._old_ft)
                                          (set vim.b._old_ft nil)))
                                      {})
