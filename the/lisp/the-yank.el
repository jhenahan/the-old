;;; the-yank.el --- Killing and yanking

(delete-selection-mode 1)

(with-eval-after-load 'latex
  (put 'LaTeX-insert-left-brace 'delete-selection t))

(setq kill-do-not-save-duplicates t)

(provide 'the-yank)

;;; the-yank.el ends here
