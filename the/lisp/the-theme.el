;;; the-theme.el --- Loading color themes

(require 'the-custom)
(require 'the-package)

(defun the--unlispify (name)
  "Converts \"deep-blue\" to \"Deep Blue\"."
  (capitalize
   (replace-regexp-in-string
    "-" " " name)))

(defmacro the-with-color-theme (theme &rest body)
  "If the current color theme is THEME, eval BODY; else return nil.
The current color theme is determined by consulting
`the-color-theme'."
  (declare (indent 1))
  ;; `theme' should be a symbol so we can use `eq'.
  `(when (eq ',theme the-color-theme)
     ,@body))

(defcustom the-color-theme 'gruvbox
  "Specifies the color theme used by The.
You can use anything listed by `custom-available-themes'. If you
wish to use your own color theme, you can set this to nil."
  :group 'the
  :type `(choice ,@(mapcar (lambda (theme)
			     `(const :tag
				     ,(if theme
					  (the--unlispify
					   (symbol-name theme))
					"None")
				     ,theme))
			   (cons
			    nil
			    (sort
			     (append
			      (custom-available-themes)
			      '(leuven
				gruvbox))
			     #'string-lessp)))))

(defcustom the-defer-color-theme t
  "Non-nil means defer loading the color theme until after init.
Otherwise, the color theme is loaded whenever `the-theme' is
loaded."
  :group 'the
  :type 'boolean)

(the-with-color-theme leuven
  (set-face-background 'highlight "#B1EAFC")
  (set-face-underline 'isearch nil)
  (set-face-background 'lazy-highlight "#B1EAFC")
  (set-face-underline 'lazy-highlight nil)
  (set-face-underline 'show-paren-mismatch nil))

(straight-register-package 'gruvbox-theme)
(the-with-color-theme gruvbox
  (use-package gruvbox-theme))

(when the-color-theme
  (if the-defer-color-theme
      (progn
	(eval-and-compile
	  (defun the-load-color-theme ()
	    "Load the The color theme, as given by `the-color-theme'.
If there is an error, report it as a warning."
	    (condition-case-unless-debug error-data
		(load-theme the-color-theme 'no-confirm)
	      (error (warn "Could not load color theme: %s"
			   (error-message-string error-data))))))
	(add-hook 'after-init-hook #'the-load-color-theme))
    (load-theme the-color-theme 'no-confirm)))

(provide 'the-theme)

;;; the-theme.el ends here
