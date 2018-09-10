;; -*- lexical-binding: t; -*-
;;; the-lisp.el --- Support for Lisps

(require 'the-indent)

(add-hook 'lisp-mode-hook #'aggressive-indent-mode)

(provide 'the-lisp)

;;; the-lisp.el ends here
