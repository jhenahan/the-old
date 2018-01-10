;;; the-clipboard.el --- Integration with the system clipboard

(require 'the-os)
(require 'the-windowed)

(the-with-operating-system macOS
  (the-with-terminal-emacs
    (defvar the-clipboard-last-copy nil
      "The last text that was copied to the system clipboard.
This is used to prevent duplicate entries in the kill ring.")

    (defun the-clipboard-paste ()
      "Return the contents of the macOS clipboard, as a string."
      (let* (;; Setting `default-directory' to a directory that is
             ;; sure to exist means that this code won't error out
             ;; when the directory for the current buffer does not
             ;; exist.
             (default-directory "/")
             ;; Command pbpaste returns the clipboard contents as a
             ;; string.
             (text (shell-command-to-string "pbpaste")))
        ;; If this function returns nil then the system clipboard is
        ;; ignored and the first element in the kill ring (which, if
        ;; the system clipboard has not been modified since the last
        ;; kill, will be the same). Including this `unless' clause
        ;; prevents you from getting the same text yanked the first
        ;; time you run `yank-pop'. (Of course, this is less relevant
        ;; due to `counsel-yank-pop', but still arguably the correct
        ;; behavior.)
        (unless (string= text the-clipboard-last-copy)
          text)))

    (defun the-clipboard-copy (text)
      "Set the contents of the macOS clipboard to given TEXT string."
      (let* (;; Setting `default-directory' to a directory that is
             ;; sure to exist means that this code won't error out
             ;; when the directory for the current buffer does not
             ;; exist.
             (default-directory "/")
             ;; Setting `process-connection-type' makes Emacs use a pipe to
             ;; communicate with pbcopy, rather than a pty (which is
             ;; overkill).
             (process-connection-type nil)
             ;; The nil argument tells Emacs to discard stdout and
             ;; stderr. Note, we aren't using `call-process' here
             ;; because we want this command to be asynchronous.
             ;;
             ;; Command pbcopy writes stdin to the clipboard until it
             ;; receives EOF.
             (proc (start-process "pbcopy" nil "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))
      (setq the-clipboard-last-copy text))

    (setq interprogram-paste-function #'the-clipboard-paste)
    (setq interprogram-cut-function #'the-clipboard-copy)))

(setq save-interprogram-paste-before-kill t)

(provide 'the-clipboard)

;;; the-clipboard.el ends here
