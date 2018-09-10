;; -*- lexical-binding: t; -*-
;;; the-langs.el --- programming is cool

(require 'the-package)

(use-package gnu-apl-mode)

(use-package haskell-mode
  :commands haskell-mode
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '((;company-capf
                              company-dabbrev-code))
                           company-backends)))))

(use-package dante
  :after haskell-mode
  :commands dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  :config
  (add-hook 'dante-mode-hook
            '(lambda () (flycheck-add-next-checker 'haskell-dante
                                              '(warning . haskell-hlint)))))

(use-package shm
  :after haskell-mode
  :commands structured-haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'structured-haskell-mode))

(use-package rust-mode
  :init
  (setq rust-format-on-save t))
(use-package racer
  :commands racer-mode
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package x86-lookup
  :config
  (setq x86-lookup-pdf "~/Dropbox/intel-vol2.pdf"))

(use-package nasm-mode
  :hook asm-mode)

(provide 'the-langs)

;;; the-langs.el ends here
