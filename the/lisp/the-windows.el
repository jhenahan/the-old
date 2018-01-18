;;; the-windows.el --- Managing windows
(require 'the-package)

(use-package golden-ratio
  :demand t
  :config
  (golden-ratio-mode t))

(use-package shackle
  :demand t
  :config
  (setq shackle-rules '(
                        (compilation-mode :noselect t)
                        (magit-diff-mode :noselect t)
                        ("*Warnings*" :noselect t)
                        )
        shackle-default-rule '(:select t))
  (shackle-mode 1))

(provide 'the-windows)

;;; the-windows.el ends here
