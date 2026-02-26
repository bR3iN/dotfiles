(fn sort-lenses [lenses]
  (table.sort lenses
              (fn [a b]
                (< a.range.start.line b.range.start.line)))
  lenses)

(fn current-line []
  (- (. (vim.api.nvim_win_get_cursor 0) 1) 1))

(fn find-next-lens [lenses line]
  (var result nil)
  (each [_ lens (ipairs lenses) &until result]
    (when (> lens.range.start.line line)
      (set result lens)))
  (or result (. lenses 1)))

(fn find-prev-lens [lenses line]
  (var result nil)
  (for [i (length lenses) 1 -1 &until result]
    (when (< (. lenses i :range :start :line) line)
      (set result (. lenses i))))
  (or result (. lenses (length lenses))))

(fn actionable-lenses []
  (icollect [_ {: lens} (ipairs (vim.lsp.codelens.get {:bufnr 0}))]
    (when lens.command lens)))

(fn goto-codelens [forward?]
  (let [lenses (sort-lenses (actionable-lenses))]
    (if (vim.tbl_isempty lenses)
        (vim.notify "No code lenses available" vim.log.levels.INFO)
        (let [target ((if forward? find-next-lens find-prev-lens) lenses
                                                                  (current-line))]
          (vim.api.nvim_win_set_cursor 0 [(+ target.range.start.line 1) 0])))))

{:next #(goto-codelens true) :prev #(goto-codelens false)}
