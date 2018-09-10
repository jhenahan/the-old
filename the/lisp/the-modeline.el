;; -*- lexical-binding: t; -*-
;;; the-modeline.el --- Modeline configuration

(require 'the-package)

(use-package diminish
  :demand t
  :config
  (with-eval-after-load 'subword
    (diminish 'subword-mode))
  (diminish 'filladapt-mode)
  (with-eval-after-load 'outline
    (diminish 'outline-minor-mode))
  (diminish 'smerge-mode)
  (diminish 'whitespace-mode)
  (diminish 'abbrev-mode)
  (diminish 'auto-fill-function)
  (with-eval-after-load 'page-break-lines
    (diminish 'page-break-lines-mode))
  (diminish 'visual-line-mode "ω")
  (diminish 'eldoc-mode "ε")
  )

(use-package delight
  :demand t
  :config
  (delight
   '((emacs-lisp-mode "ξ" :major))))

(use-package nyan-mode
  :demand t
  :init
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t)
  :config
  (nyan-mode 1))

(use-package spaceline
  :init
  (require 'spaceline-config)
  :config
  (spaceline-spacemacs-theme))

(provide 'the-modeline)

;;; the-modeline.el ends here
