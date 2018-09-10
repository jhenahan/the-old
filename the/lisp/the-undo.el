;; -*- lexical-binding: t; -*-
;;; the-undo.el -- Better undo interaction
(require 'the-package)

(use-package undo-tree
  :demand t
  :config
  (global-undo-tree-mode)
  :delight undo-tree-mode
  )

(provide 'the-undo)

;;; the-undo.el ends here
