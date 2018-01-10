;;; the-indent.el --- Indentation

(require 'the-package)
(require 'the-slow)

(use-package aggressive-indent
  :init
  (put 'aggressive-indent-mode 'safe-local-variable #'booleanp)
  :config
  (defun the-aggressive-indent-toggle-slow ()
    "Slow down `aggressive-indent' by disabling reindentation on save.
  This is done in `the-slow-indent-mode'."
    (add-hook 'aggressive-indent-mode-hook
              #'the-aggressive-indent-toggle-slow)
    (if (or the-slow-indent-mode (not aggressive-indent-mode))
        (remove-hook 'before-save-hook
                     #'aggressive-indent--proccess-changed-list-and-indent
                     'local)
      (add-hook 'before-save-hook
                #'aggressive-indent--proccess-changed-list-and-indent
                nil 'local)))
  
  (add-hook 'the-slow-indent-mode #'the-aggressive-indent-toggle-slow)
  :delight (aggressive-indent-mode "AggrIndent"))

(provide 'the-indent)

;;; the-indent.el ends here
