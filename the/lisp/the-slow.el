;; -*- lexical-binding: t; -*-
;;; the-slow.el --- For when Emacs just isn't fast enough

(use-package auto-compile
  :init
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (setq auto-compile-update-autoloads t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(define-minor-mode the-slow-indent-mode
  "Minor mode for when the indentation code is slow.
This prevents `aggressive-indent' from indenting as frequently.")

(define-minor-mode the-slow-autocomplete-mode
  "Minor mode for when the autocompletion code is slow.
This prevents `company' and `eldoc' from displaying metadata as
quickly.")

(provide 'the-slow)

;;; the-slow.el ends here
