;; -*- lexical-binding: t; -*-
;;; the-eldoc.el --- Eldoc customizations

(require 'the-slow)

(setq eldoc-idle-delay 0)

(setq eldoc-echo-area-use-multiline-p nil)

(setq eldoc-minor-mode-string nil)

(defun the-eldoc-toggle-slow ()
  "Slow down `eldoc' by turning up the delay before metadata is shown.
This is done in `the-slow-autocomplete-mode'."
  (if the-slow-autocomplete-mode
      (setq-local eldoc-idle-delay 1)
    (kill-local-variable 'eldoc-idle-delay)))

(add-hook 'the-slow-autocomplete-mode-hook #'the-eldoc-toggle-slow)

(provide 'the-eldoc)

;;; the-eldoc.el ends here
