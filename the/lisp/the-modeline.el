;;; the-modeline.el --- Modeline configuration

(require 'the-package)

(use-package diminish
  :demand t)

(use-package delight
  :demand t
  :delight
  (abbrev-mode)
  (auto-fill-function)
  (eldoc-mode "ε")
  (emacs-lisp-mode "ξ")
  (filladapt-mode)
  (outline-minor-mode)
  (smerge-mode)
  (subword-mode)
  (undo-tree-mode)
  (visual-line-mode "ω")
  (which-key-mode)
  (whitespace-mode)
  )

(use-package nyan-mode
  :demand t
  :init
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t)
  :config
  (nyan-mode 1))

(use-package spaceline-all-the-icons
  :demand t
  :init
  (let ((fonts
         '("all-the-icons"
           "file-icons"
           "FontAwesome"
           "github-octicons"
           "Material Icons")))
    (unless `(and
              (find-font (font-spec :name ,fonts)))
      (all-the-icons-install-fonts)))
  (setq spaceline-all-the-icons-icon-set-modified 'toggle)
  (setq spaceline-all-the-icons-icon-set-bookmark 'heart)
  (setq spaceline-all-the-icons-icon-set-flycheck-slim 'dots)
  (setq spaceline-all-the-icons-hide-long-buffer-path t)
  :config
  (spaceline-all-the-icons-theme)
  (defface the-spaceline-modalka-off
    '((t (:background "chartreuse3"
          :foreground "#3E3D31")))
    "Modalka inactive face."
    :group 'the)
  
  (defface the-spaceline-modalka-on
    '((t (:background "DarkGoldenrod2"
          :foreground "#3E3D31")))
    "Modalka inactive face."
    :group 'the)
  
  (defun the-spaceline-modalka-highlight ()
    (if modalka-mode
        'the-spaceline-modalka-on
      'the-spaceline-modalka-off))
  
  (setq spaceline-highlight-face-func #'the-spaceline-modalka-highlight)
  (spaceline-toggle-all-the-icons-nyan-cat-on)
  )

(provide 'the-modeline)

;;; the-modeline.el ends here
