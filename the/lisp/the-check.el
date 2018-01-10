;;; the-check.el --- On-the-fly syntax and semantics checking

(require 'the-package)

(use-package flycheck
  :defer 3
  :config
  (global-flycheck-mode +1)
  (put 'flycheck-mode 'safe-local-variable #'booleanp)
  (setq flycheck-mode-line nil)
  )

(provide 'the-check)

;;; the-check.el ends here
