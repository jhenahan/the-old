;;; the-autocomplete.el --- In-buffer completion

(require 'the-package)
(require 'the-patch)
(require 'the-slow)

(defvar the-company-backends-global
  '(company-capf
    company-files
    (company-dabbrev-code company-keywords)
    company-dabbrev)
  "Values for `company-backends' used everywhere.
If `company-backends' is overridden by The, then these
backends will still be included.")

(use-package company
  :demand t
  :config
  (company-tng-configure-default)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-limit 10)
  (setq company-tooltip-minimum company-tooltip-limit)
  (setq company-show-numbers t)
  (setq company-auto-complete-chars nil)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-other-buffers nil)
  (setq company-dabbrev-ignore-case nil)
  
  (defun the-company-toggle-slow ()
    "Slow down `company' by turning up the delays before completion starts.
  This is done in `the-slow-autocomplete-mode'."
    (if the-slow-autocomplete-mode
        (progn
          (setq-local company-idle-delay 1)
          (setq-local company-minimum-prefix-length 3))
      (kill-local-variable 'company-idle-delay)
      (kill-local-variable 'company-minimum-prefix-length)))
  
  (add-hook 'the-slow-autocomplete-mode-hook #'the-company-toggle-slow)
  (global-company-mode +1)
  :delight company-mode)

(use-package company-statistics
  :demand t
  :config

  ;; Let's future-proof our patching here just in case we ever decide
  ;; to lazy-load company-statistics.
  (el-patch-feature company-statistics)

  ;; Disable the message that is normally printed when
  ;; `company-statistics' loads its statistics file from disk.
  (el-patch-defun company-statistics--load ()
    "Restore statistics."
    (load company-statistics-file 'noerror
          (el-patch-swap nil 'nomessage)
          'nosuffix))

  ;; Enable Company Statistics.
  (company-statistics-mode +1))

(provide 'the-autocomplete)

;;; the-autocomplete.el ends here
