;; -*- lexical-binding: t; -*-
;;; the-clipboard.el --- Integration with the system clipboard

(require 'the-os)
(require 'the-windowed)

(the-with-operating-system macOS
  (use-package osx-clipboard
    :demand t
    :diminish
    :config
    (osx-clipboard-mode +1)))

(setq save-interprogram-paste-before-kill t)

(provide 'the-clipboard)

;;; the-clipboard.el ends here
