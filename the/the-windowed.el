;;; the-windowed.el --- Checking the window system

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utility functions for checking the window system

(defmacro the-with-windowed-emacs (&rest body)
  "Eval BODY if Emacs is windowed, else return nil."
  (declare (indent defun))
  `(when (display-graphic-p)
     ,@body))

(defmacro the-with-terminal-emacs (&rest body)
  "Eval BODY if Emacs is not windowed, else return nil."
  (declare (indent defun))
  `(unless (display-graphic-p)
     ,@body))

(provide 'the-windowed)

;;; the-windowed.el ends here
