;;; the-sensible.el --- Sensible defaults

(setq default-directory "~/")

(add-hook 'prog-mode-hook 'subword-mode)

(setq gc-cons-threshold 20000000)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(transient-mark-mode t)

(fset #'yes-or-no-p #'y-or-n-p)

(the-with-operating-system macOS
  (setq ns-pop-up-frames nil))

(provide 'the-sensible)

;;; the-sensible.el ends here
