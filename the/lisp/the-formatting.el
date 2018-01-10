;;; the-formatting.el --- Formatting text

(require 'the-appearance)
(require 'the-package)

(setq-default indent-tabs-mode nil)

(setq sentence-end-double-space nil)

(defvar the-delete-trailing-whitespace t
  "If non-nil, delete trailing whitespace on save.")

(put 'the-delete-trailing-whitespace
     'safe-local-variable #'booleanp)

(defun the--maybe-delete-trailing-whitespace ()
  "Maybe delete trailing whitespace in buffer.
Trailing whitespace is only deleted if variable
`the-delete-trailing-whitespace' if non-nil."
  (when the-delete-trailing-whitespace
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook
          #'the--maybe-delete-trailing-whitespace)

(setq require-final-newline t)

(define-minor-mode the-long-lines-mode
  "When enabled, highlight long lines."
  nil nil nil
  (if the-long-lines-mode
      (progn
        (setq-local whitespace-style '(face lines-tail))
        (whitespace-mode 1))
    (whitespace-mode -1)
    (kill-local-variable 'whitespace-style)))

(add-hook 'text-mode-hook #'auto-fill-mode)

(use-package filladapt
  :demand t
  :config
  (add-hook 'text-mode-hook #'filladapt-mode)
  (add-hook 'org-mode-hook #'turn-off-filladapt-mode))

(use-package adaptive-wrap
  :demand t
  :config
  (define-globalized-minor-mode global-adaptive-wrap-prefix-mode
    adaptive-wrap-prefix-mode adaptive-wrap-prefix-mode)

  (global-adaptive-wrap-prefix-mode))

(use-package editorconfig)

(defun the-reverse-characters (beg end)
  "Reverse the characters in the region from BEG to END.
Interactively, reverse the characters in the current region."
  (interactive "*r")
  (insert
   (reverse
    (delete-and-extract-region
     beg end))))

(provide 'the-formatting)

;;; the-formatting.el ends here
