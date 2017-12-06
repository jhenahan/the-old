;;; the-os.el --- Operating system detection

(require 'the-custom)

(defcustom the-operating-system
  (pcase system-type
    ('darwin 'macOS)
    ((or 'ms-dos 'windows-nt 'cygwin) 'windows)
    (_ 'linux))
  "Specifies the operating system.
This can be `macOS', `linux', or `windows'. Normally this is
automatically detected and does not need to be changed."
  :group 'the
  :type '(choice (const :tag "macOS" macOS)
                 (const :tag "Windows" windows)
                 (const :tag "Linux" linux)))

;; This macro allows us to handle things like operating system
;; specific clipboard/mouse hacks and other things like that.
(defmacro the-with-operating-system (os &rest body)
  "If the operating system is OS, eval BODY.
See `the-operating-system' for the possible values of OS,
which should not be quoted."
  (declare (indent 1))
  `(when (eq the-operating-system ',os)
     ,@body))

(provide 'the-os)

;;; the-os.el ends here
