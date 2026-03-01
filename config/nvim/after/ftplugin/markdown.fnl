(local {: buf-opts! : keymaps!} (require :utils))

;; Note: backup and writebackup are global options and cannot be set buffer-locally
;; (buf-opts! {:backup false :writebackup false})
(buf-opts! {:iskeyword+ "\\"})

(vim.cmd.abbreviate ["<buffer>" "\\bf" "\\mathbf"])
(vim.cmd.abbreviate ["<buffer>" "\\rm" "\\mathrm"])
(vim.cmd.abbreviate ["<buffer>" "\\cal" "\\mathcal"])
(vim.cmd.abbreviate ["<buffer>" "\\bb" "\\mathbb"])
(vim.cmd.abbreviate ["<buffer>" "\\frak" "\\mathfrak"])
(vim.cmd.abbreviate ["<buffer>" "iff" "if and only if"])

(fn insert-url-link [url]
  "Fetch title for URL and insert markdown link at cursor"
  (let [result (: (vim.system [:get-url-title url]) :wait)]
    (if (= result.code 0)
        (let [title (string.gsub result.stdout "^%s*(.-)%s*$" "%1")
              link (.. "[" title "](" url ")")]
          (vim.api.nvim_put [link] "" true true))
        (vim.notify (.. "Failed to fetch title for: " url) vim.log.levels.ERROR))))

(fn insert-url-link-prompt []
  "Prompt for URL and insert link"
  (vim.ui.input {:prompt "URL: "}
                (fn [url]
                  (when (and url (not= url ""))
                    (insert-url-link url)))))

(fn insert-url-link-clipboard []
  "Insert link from clipboard URL"
  (let [url (vim.fn.getreg "+")]
    (if (not= url "")
        (insert-url-link url)
        (vim.notify "Clipboard is empty" vim.log.levels.WARN))))

(keymaps! {:opts {:buffer true}
           :n {:<localleader> {:i {:desc "Insert URL link (prompt)"
                                   :callback insert-url-link-prompt}
                               :p {:desc "Insert URL link (clipboard)"
                                   :callback insert-url-link-clipboard}}}})
