;; -*- lexical-binding: t; -*-
;;; the-config.el --- Configuration file editing

(require 'the-package)
(require 'the-regexp)

(use-package apache-mode)

(use-package dockerfile-mode)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package ssh-config-mode)

(use-package yaml-mode
  :init
  (setq the--ansible-filename-re ".*\\(main\.yml\\|site\.yml\\|encrypted\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\)")
  (add-to-list 'auto-mode-alist `(,the--ansible-filename-re . yaml-mode))
  :config
  (defun the--disable-auto-fill-mode ()
    (auto-fill-mode -1))

  (add-hook 'yaml-mode-hook #'the--disable-auto-fill-mode))

(use-package jinja2-mode)

(provide 'the-config)

;;; the-config.el ends here
