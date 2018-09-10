;; -*- lexical-binding: t; -*-
;;; the-prose.el --- Useful utilities for writing prose

(require 'the-package)

(use-package tex
  :demand t
  :straight auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

(use-package tex-buf
  :straight auctex
  :config

  ;; Save buffers automatically when compiling, instead of prompting.
  (setq TeX-save-query nil))

(use-package latex
  :straight auctex
  :config

  ;; Don't be afraid to break inline math between lines.
  (setq LaTeX-fill-break-at-separators nil))

(use-package auctex-latexmk
  :demand t
  :after tex
  :config
  (auctex-latexmk-setup))

(use-package company-auctex
  :demand t
  :after tex
  :config

  ;; Enable the functionality of `company-auctex'.
  (company-auctex-init))

(use-package flyspell
  :bind* (("M-T ] s" . flyspell-goto-next-error))
  :diminish (flyspell-mode . "φ"))

(provide 'the-prose)

;;; the-prose.el ends here
