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
  :init (company-tng-configure-default)
  :config
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 3)
    (setq company-tooltip-limit 10)
    (setq company-tooltip-minimum company-tooltip-limit)
    (setq company-show-numbers t)
    (setq company-auto-complete-chars nil)
    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-other-buffers nil)
  
  
    (setq company-dabbrev-ignore-case nil)
  
    ;; Register `company' in `the-slow-autocomplete-mode'.
  
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
  
    ;; Make it so that Company's keymap overrides Yasnippet's keymap
    ;; when a snippet is active. This way, you can TAB to complete a
    ;; suggestion for the current field in a snippet, and then TAB to
    ;; move to the next field. Plus, C-g will dismiss the Company
    ;; completions menu rather than cancelling the snippet and moving
    ;; the cursor while leaving the completions menu on-screen in the
    ;; same location.
    (with-eval-after-load 'yasnippet
      ;; FIXME: this is all a horrible hack, can it be done with
      ;; `bind-key' instead?
      ;;
      ;; This function translates the "event types" I get from
      ;; `map-keymap' into things that I can pass to `lookup-key'
      ;; and `define-key'. It's a hack, and I'd like to find a
      ;; built-in function that accomplishes the same thing while
      ;; taking care of any edge cases I might have missed in this
      ;; ad-hoc solution.
      (defun the-normalize-event (event)
        "This function is a complete hack, do not use.
  But in principle, it translates what we get from `map-keymap'
  into what `lookup-key' and `define-key' want."
        (if (vectorp event)
            event
          (vector event)))
  
      ;; Here we define a hybrid keymap that delegates first to
      ;; `company-active-map' and then to `yas-keymap'.
      (setq the-yas-company-keymap
            ;; It starts out as a copy of `yas-keymap', and then we
            ;; merge in all of the bindings from
            ;; `company-active-map'.
            (let ((keymap (copy-keymap yas-keymap)))
              (map-keymap
               (lambda (event company-cmd)
                 (let* ((event (the-normalize-event event))
                        (yas-cmd (lookup-key yas-keymap event)))
                   ;; Here we use an extended menu item with the
                   ;; `:filter' option, which allows us to
                   ;; dynamically decide which command we want to
                   ;; run when a key is pressed.
                   (define-key keymap event
                     `(menu-item
                       nil ,company-cmd :filter
                       (lambda (cmd)
                         ;; There doesn't seem to be any obvious
                         ;; function from Company to tell whether or
                         ;; not a completion is in progress (à la
                         ;; `company-explicit-action-p'), so I just
                         ;; check whether or not `company-my-keymap'
                         ;; is defined, which seems to be good
                         ;; enough.
                         (if company-my-keymap
                             ',company-cmd
                           ',yas-cmd))))))
               company-active-map)
              keymap))
  
      ;; The function `yas--make-control-overlay' uses the current
      ;; value of `yas-keymap' to build the Yasnippet overlay, so to
      ;; override the Yasnippet keymap we only need to dynamically
      ;; rebind `yas-keymap' for the duration of that function.
      (defun the-advice-company-overrides-yasnippet
          (yas--make-control-overlay &rest args)
        "Allow `company' to override `yasnippet'.
  This is an `:around' advice for `yas--make-control-overlay'."
        (let ((yas-keymap the-yas-company-keymap))
          (apply yas--make-control-overlay args)))
  
      (advice-add #'yas--make-control-overlay :around
                  #'the-advice-company-overrides-yasnippet))
  
    ;; Turn on Company everywhere.
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
