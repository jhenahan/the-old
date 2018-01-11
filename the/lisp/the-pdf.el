;;; the-pdf.el --- PDF functionality

(require 'the-package)

(use-package pdf-tools
  :init
  (pdf-tools-install)
  (setq pdf-view-midnight-colors '("#fe8019" . "#1d2021"))
  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))

(provide 'the-pdf)

;;; the-pdf.el ends here
