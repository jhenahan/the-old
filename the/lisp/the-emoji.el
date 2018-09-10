;; -*- lexical-binding: t; -*-
;;; the-emoji.el --- Emojis :smile:

(require 'the-package)

(use-package emojify
  :init
  (add-hook 'after-init-hook #'global-emojify-mode))

(provide 'the-emoji)

;;; the-emoji.el ends here
