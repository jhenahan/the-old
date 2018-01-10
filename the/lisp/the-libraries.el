;;; the-libraries.el --- Helfpul utility libraries

(require 'the-package)

(use-package async
  :commands (async-start))

(use-package cl-lib
  :demand t)

(use-package dash
  :demand t)

(use-package f
  :demand t)

(use-package s
  :demand t)

(use-package ht
  :demand t)

(require 'subr-x)

(provide 'the-libraries)

;;; the-libraries.el ends here
