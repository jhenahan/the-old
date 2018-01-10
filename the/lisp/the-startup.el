;;; the-startup.el --- Cleaning up Emacs startup

(defalias 'the--advice-inhibit-startup-echo-area-message #'ignore
  "Unconditionally inhibit the startup message in the echo area.
This is an `:override' advice for
`display-startup-echo-area-message'.")

(advice-add #'display-startup-echo-area-message :override
            #'the--advice-inhibit-startup-echo-area-message)

(use-package dashboard
  :demand t
  :init
  (setq dashboard-banner-logo-title "REPENT!")
  (setq dashboard-startup-banner (f-expand "heresy.png" the-image-directory))
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook))

(setq initial-scratch-message nil)

(server-start)

(provide 'the-startup)

;;; the-startup.el ends here
