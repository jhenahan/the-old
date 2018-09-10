;; -*- lexical-binding: t; -*-
;;; the-elisp.el --- Support for Emacs Lisp

(require 'the-bind-key)
(require 'the-check)
(require 'the-custom)
(require 'the-eldoc)
(require 'the-indent)
(require 'the-lisp)
(require 'the-package)
(require 'the-patch)

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(setq ad-redefinition-action 'accept)

(defcustom the-reload-init-keybinding
  (the-join-keys the-prefix "r")
  "The keybinding for reloading init.el, as a string.
Nil means no keybinding is established."
  :group 'the
  :type 'string)

(defun the-reload-init ()
  "Reload init.el."
  (interactive)
  (straight-transaction
    (straight-mark-transaction-as-init)
    (message "Reloading init.el...")
    (load user-init-file nil 'nomessage)
    (message "Reloading init.el... done.")))

(bind-key the-reload-init-keybinding #'the-reload-init)

(defun the-eval-buffer ()
  "Evaluate the current buffer as Elisp code."
  (interactive)
  (message "Evaluating %s..." (buffer-name))
  (straight-transaction
    (if (null buffer-file-name)
        (eval-buffer)
      (when (string= buffer-file-name user-init-file)
        (straight-mark-transaction-as-init))
      (load buffer-file-name nil 'nomessage)))
  (message "Evaluating %s... done." (buffer-name)))

(bind-key "C-c C-k" #'the-eval-buffer emacs-lisp-mode-map)

(defun find-symbol (&optional symbol)
  "Same as `xref-find-definitions' but only for Elisp symbols."
  (interactive)
  (let ((xref-backend-functions '(elisp--xref-backend)))
    (if symbol
        (xref-find-definitions symbol)
      (call-interactively 'xref-find-definitions))))

(bind-keys
 ("C-h C-f" . find-function)
 ("C-h C-v" . find-variable)
 ("C-h C-o" . find-symbol))

(defun the--rename-lisp-interaction-mode ()
  (setq mode-name "ξι"))

(add-hook 'lisp-interaction-mode-hook
          #'the--rename-lisp-interaction-mode)

(provide 'the-elisp)

;;; the-elisp.el ends here
