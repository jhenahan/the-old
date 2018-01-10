;;; the-network.el --- Network and web browsing

(require 'the-os)
(require 'the-package)

(the-with-operating-system macOS
  (with-eval-after-load 'gnutls
    (setq gnutls-verify-error t)
    (setq gnutls-min-prime-bits 3072)
    (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem")))

(use-package sx)

(put 'bug-reference-bug-regexp 'safe-local-variable #'stringp)

(use-package ix)

(use-package eww
  :bind* (("M-T g x" . eww)
          ("M-T g :" . eww-browse-with-external-browser)
          ("M-T g #" . eww-list-histories)
          ("M-T g {" . eww-back-url)
          ("M-T g }" . eww-forward-url))
  :config
  (progn
    (add-hook 'eww-mode-hook 'visual-line-mode)))

(provide 'the-network)

;;; the-network.el ends here
