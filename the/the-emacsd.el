;;; the-emacsd.el --- Organizing ~/.emacs.d

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Organizing ~/.emacs.d

(require 'the-package)

;; This package changes the default paths for lots of different
;; packages, with the net result that the ~/.emacs.d folder is much
;; more clean and organized. See the README [1].
;;
;; [1]: https://github.com/raxod502/no-littering
(use-package no-littering
  :demand t)

(provide 'the-emacsd)

;;; the-emacsd.el ends here
