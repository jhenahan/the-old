;; -*- lexical-binding: t; -*-
;;; the-bind-key.el --- Utility functions for keybindings

(require 'the-custom)
(require 'the-package)
(require 'the-libraries)

(defcustom the-prefix "M-T"
  "Prefix key sequence for The-related keybindings.
This is a string as would be passed to `kbd'."
  :group 'the
  :type 'string)

(defun the-join-keys (&rest keys)
  "Join key sequences. Empty strings and nils are discarded.
\(the--join-keys \"M-P e\" \"e i\") => \"M-P e e i\"
\(the--join-keys \"M-P\" \"\" \"e i\") => \"M-P e i\""
  (string-join (remove "" (mapcar #'string-trim (remove nil keys))) " "))

(use-package bind-key)

(provide 'the-bind-key)

;;; the-bind-key.el ends here
