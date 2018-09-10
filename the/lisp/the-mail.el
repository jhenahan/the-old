;; -*- lexical-binding: t; -*-
;;; the-mail.el --- Email

(require 'the-package)

(use-package gnus
  :init
  (setq user-mail-address "jhenahan@me.com"
        user-full-name "Jack Henahan")
  (setq gnus-select-method
        '(nnimap "iCloud"
                 (nnimap-address "imap.mail.me.com")
                 (nnimap-server-port "imaps")
                 (nnimap-stream ssl)))
  (setq smtpmail-smtp-server "smtp.mail.me.com"
        smtpmail-smtp-service 587
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  )

(provide 'the-mail)

;;; the-mail.el ends here
