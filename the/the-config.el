;;; the-config.el --- Configuration file editing

(require 'the-package)
(require 'the-regexp)

;; Edit .htaccess and friends.
(use-package apache-mode)

;; Edit Dockerfiles.
(use-package dockerfile-mode)

;; Edit .gitconfig and .gitmodules files.
(use-package gitconfig-mode)

;; Edit .gitignore files.
(use-package gitignore-mode)

;; Package `ssh-config-mode' provides syntax highlighting and
;; indentation for files in ~/.ssh.
(use-package ssh-config-mode)

;; Provides syntax highlighting, indentation, and editing commands for
;; YAML files.
(use-package yaml-mode
  :init
  (setq the--ansible-filename-re ".*\\(main\.yml\\|site\.yml\\|encrypted\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\)")
  (add-to-list 'auto-mode-alist `(,the--ansible-filename-re . yaml-mode))
  :config

  ;; Don't automatically wrap text when editing YAML files.

  (defun the--disable-auto-fill-mode ()
    (auto-fill-mode -1))

  (add-hook 'yaml-mode-hook #'the--disable-auto-fill-mode))

(use-package jinja2-mode)

(provide 'the-config)

;;; the-config.el ends here
