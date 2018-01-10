;;; the-elisp.el --- Support for Emacs Lisp

(require 'the-bind-key)
(require 'the-check)
(require 'the-custom)
(require 'the-eldoc)
(require 'the-indent)
(require 'the-lisp)
(require 'the-package)
(require 'the-patch)

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(setq ad-redefinition-action 'accept)

(el-patch-defun lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (el-patch-let (($cond (and (elt state 2)
                             (el-patch-wrap 1 1
                               (or (not (looking-at "\\sw\\|\\s_"))
                                   (looking-at ":")))))
                 ($then (progn
                          (if (not (> (save-excursion (forward-line 1) (point))
                                      calculate-lisp-indent-last-sexp))
                              (progn (goto-char calculate-lisp-indent-last-sexp)
                                     (beginning-of-line)
                                     (parse-partial-sexp (point)
                                                         calculate-lisp-indent-last-sexp 0 t)))
                          ;; Indent under the list or under the first sexp on the same
                          ;; line as calculate-lisp-indent-last-sexp.  Note that first
                          ;; thing on that line has to be complete sexp since we are
                          ;; inside the innermost containing sexp.
                          (backward-prefix-chars)
                          (current-column)))
                 ($else (let ((function (buffer-substring (point)
                                                          (progn (forward-sexp 1) (point))))
                              method)
                          (setq method (or (function-get (intern-soft function)
                                                         'lisp-indent-function)
                                           (get (intern-soft function) 'lisp-indent-hook)))
                          (cond ((or (eq method 'defun)
                                     (and (null method)
                                          (> (length function) 3)
                                          (string-match "\\`def" function)))
                                 (lisp-indent-defform state indent-point))
                                ((integerp method)
                                 (lisp-indent-specform method state
                                                       indent-point normal-indent))
                                (method
                                 (funcall method indent-point state))))))
    (let ((normal-indent (current-column))
          (el-patch-add
            (orig-point (point))))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (el-patch-swap
        (if $cond
            ;; car of form doesn't seem to be a symbol
            $then
          $else)
        (cond
         ;; car of form doesn't seem to be a symbol, or is a keyword
         ($cond $then)
         ((and (save-excursion
                 (goto-char indent-point)
                 (skip-syntax-forward " ")
                 (not (looking-at ":")))
               (save-excursion
                 (goto-char orig-point)
                 (looking-at ":")))
          (save-excursion
            (goto-char (+ 2 (elt state 1)))
            (current-column)))
         (t $else))))))

(defcustom the-reload-init-keybinding
  (the-join-keys the-prefix "r")
  "The keybinding for reloading init.el, as a string.
Nil means no keybinding is established."
  :group 'the
  :type 'string)

(defun the-reload-init ()
  "Reload init.el."
  (interactive)
  (straight-transaction
    (straight-mark-transaction-as-init)
    (message "Reloading init.el...")
    (load user-init-file nil 'nomessage)
    (message "Reloading init.el... done.")))

(bind-key the-reload-init-keybinding #'the-reload-init)

(defun the-eval-buffer ()
  "Evaluate the current buffer as Elisp code."
  (interactive)
  (message "Evaluating %s..." (buffer-name))
  (straight-transaction
    (if (null buffer-file-name)
        (eval-buffer)
      (when (string= buffer-file-name user-init-file)
        (straight-mark-transaction-as-init))
      (load buffer-file-name nil 'nomessage)))
  (message "Evaluating %s... done." (buffer-name)))

(bind-key "C-c C-k" #'the-eval-buffer emacs-lisp-mode-map)

(defun find-symbol (&optional symbol)
  "Same as `xref-find-definitions' but only for Elisp symbols."
  (interactive)
  (let ((xref-backend-functions '(elisp--xref-backend)))
    (if symbol
        (xref-find-definitions symbol)
      (call-interactively 'xref-find-definitions))))

(bind-keys
 ("C-h C-f" . find-function)
 ("C-h C-v" . find-variable)
 ("C-h C-o" . find-symbol))

(defun the--rename-lisp-interaction-mode ()
  (setq mode-name "ξι"))

(add-hook 'lisp-interaction-mode-hook
          #'the--rename-lisp-interaction-mode)

(provide 'the-elisp)

;;; the-elisp.el ends here
