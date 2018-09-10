;; -*- lexical-binding: t; -*-
;;; the-help.el --- better help behavior

(require 'the-package)

(use-package helpful
  :demand t
  :config
  (defalias #'describe-key #'helpful-key)
  (defalias #'describe-function #'helpful-callable)
  (defalias #'describe-variable #'helpful-variable)
  (defalias #'describe-symbol #'helpful-symbol))

(provide 'the-help)

;;; the-help.el ends here
