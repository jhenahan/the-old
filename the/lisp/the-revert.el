;;; the-revert.el --- Auto-revert

(require 'the-libraries)
(require 'the-patch)

(setq auto-revert-interval 1)

(global-auto-revert-mode 1)

(el-patch-defun auto-revert-buffers ()
  "Revert buffers as specified by Auto-Revert and Global Auto-Revert Mode.
Should `global-auto-revert-mode' be active all file buffers are checked.
Should `auto-revert-mode' be active in some buffers, those buffers
are checked.
Non-file buffers that have a custom `revert-buffer-function' and
`buffer-stale-function' are reverted either when Auto-Revert
Mode is active in that buffer, or when the variable
`global-auto-revert-non-file-buffers' is non-nil and Global
Auto-Revert Mode is active.
This function stops whenever there is user input.  The buffers not
checked are stored in the variable `auto-revert-remaining-buffers'.
To avoid starvation, the buffers in `auto-revert-remaining-buffers'
are checked first the next time this function is called.
This function is also responsible for removing buffers no longer in
Auto-Revert mode from `auto-revert-buffer-list', and for canceling
the timer when no buffers need to be checked."

  (setq auto-revert-buffers-counter
        (1+ auto-revert-buffers-counter))

  (save-match-data
    (let ((bufs (el-patch-wrap 2
                  (cl-remove-if-not
                   #'get-buffer-window
                   (if global-auto-revert-mode
                       (buffer-list)
                     auto-revert-buffer-list))))
          remaining new)
      ;; Partition `bufs' into two halves depending on whether or not
      ;; the buffers are in `auto-revert-remaining-buffers'.  The two
      ;; halves are then re-joined with the "remaining" buffers at the
      ;; head of the list.
      (dolist (buf auto-revert-remaining-buffers)
        (if (memq buf bufs)
            (push buf remaining)))
      (dolist (buf bufs)
        (if (not (memq buf remaining))
            (push buf new)))
      (setq bufs (nreverse (nconc new remaining)))
      (while (and bufs
                  (not (and auto-revert-stop-on-user-input
                            (input-pending-p))))
        (let ((buf (car bufs)))
          (if (buffer-live-p buf)
              (with-current-buffer buf
                ;; Test if someone has turned off Auto-Revert Mode in a
                ;; non-standard way, for example by changing major mode.
                (if (and (not auto-revert-mode)
                         (not auto-revert-tail-mode)
                         (memq buf auto-revert-buffer-list))
                    (setq auto-revert-buffer-list
                          (delq buf auto-revert-buffer-list)))
                (when (auto-revert-active-p)
                  ;; Enable file notification.
                  (when (and auto-revert-use-notify
                             (not auto-revert-notify-watch-descriptor))
                    (auto-revert-notify-add-watch))
                  (auto-revert-handler)))
            ;; Remove dead buffer from `auto-revert-buffer-list'.
            (setq auto-revert-buffer-list
                  (delq buf auto-revert-buffer-list))))
        (setq bufs (cdr bufs)))
      (setq auto-revert-remaining-buffers bufs)
      ;; Check if we should cancel the timer.
      (when (and (not global-auto-revert-mode)
                 (null auto-revert-buffer-list))
        (cancel-timer auto-revert-timer)
        (setq auto-revert-timer nil)))))

(setq global-auto-revert-non-file-buffers t)

(setq revert-without-query '(".*"))

(with-eval-after-load 'help-mode
  (defun the--advice-disable-help-mode-revert-prompt
      (help-mode-revert-buffer _ignore-auto _noconfirm)
    (funcall help-mode-revert-buffer _ignore-auto 'noconfirm))
  (advice-add #'help-mode-revert-buffer :around
              #'the--advice-disable-help-mode-revert-prompt))

(setq auto-revert-mode-text nil)

(provide 'the-revert)

;;; the-revert.el ends here
