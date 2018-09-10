;; -*- lexical-binding: t; -*-
;;; the-navigation.el --- Navigating within a file

(require 'cl-lib)
(require 'the-bind-key)
(require 'the-completion)
(require 'the-custom)
(require 'the-package)

;; Make it so that if you provide a negative prefix argument to
;; C-SPC (i.e. like M-- C-SPC), then it will step forwards in the mark
;; ring (whereas C-u C-SPC steps backwards). Based on [1].
;;
;; [1]: http://stackoverflow.com/a/14539202/3538165

(defun the--advice-allow-unpopping-mark
    (set-mark-command &optional arg)
  (interactive "P")
  (if (< (prefix-numeric-value arg) 0)
      ;; If we don't have any marks set, no-op.
      (when mark-ring
        ;; I can't remember how this code works. Sorry.
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring)))))
    ;; If no prefix argument, or prefix argument is nonnegative, defer
    ;; to the original behavior.
    (funcall set-mark-command arg)))

(advice-add #'set-mark-command :around
            #'the--advice-allow-unpopping-mark)

;; Make it so that if you provide a prefix argument to C-x C-SPC, then
;; it will step forwards in the global mark ring, instead of
;; backwards. Tweaked from the implementation of `pop-global-mark'.

(defun the--advice-allow-unpopping-global-mark
    (pop-global-mark &optional arg)
  (interactive "P")
  (if arg
      (progn
        (or global-mark-ring
            (error "No global mark set"))
        ;; We need to do this earlier than `pop-global-mark' does the
        ;; corresponding action in order to properly undo its
        ;; behavior.
        (setq global-mark-ring (nconc (list (car (last global-mark-ring)))
                                      (butlast global-mark-ring)))
        (while (and global-mark-ring (not (marker-buffer (car (last global-mark-ring)))))
          (setq global-mark-ring (butlast global-mark-ring)))
        (let* ((marker (car (last global-mark-ring)))
               (buffer (marker-buffer marker))
               (position (marker-position marker)))
          (set-buffer buffer)
          (or (and (>= position (point-min))
                   (<= position (point-max)))
              (if widen-automatically
                  (widen)
                (error "Global mark position is outside accessible part of buffer")))
          (goto-char position)
          (switch-to-buffer buffer)))
    (funcall pop-global-mark)))

(advice-add #'pop-global-mark :around
            #'the--advice-allow-unpopping-global-mark)

;; When using M-. and friends, always prompt for the identifier (it
;; defaults to the identifier at point). This behavior is more
;; consistent and predictable than the default, which is to jump
;; immediately if there is a valid symbol at point.
(setq xref-prompt-for-identifier t)

;; Eliminate the quarter-second delay before I-search matches are
;; highlighted, because delays suck.
(setq lazy-highlight-initial-delay 0)

;; This package provides an enhanced version of Isearch that uses Ivy
;; to display a preview of the results.
(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; This package allows you to jump quickly to characters and words
;; visible onscreen.
(use-package avy
  :init

  ;; Create keybindings for common avy commands.

  (defcustom the-avy-prefix
    (the-join-keys the-prefix "g")
    "Prefix key sequence for Avy commands.
Avy keybindings are placed under this prefix by default. nil
means no keybindings are established by default."
    :group 'the
    :type 'string)

  (defmacro the--establish-avy-bindings (&rest specs)
    `(progn
       ,@(cl-mapcan
          (lambda (spec)
            (cl-destructuring-bind (key . command-name) spec
              (let ((custom-name (intern (format "the-%S-keybinding"
                                                 command-name))))
                `((defcustom ,custom-name
                    (when the-avy-prefix
                      (the-join-keys the-avy-prefix ,key))
                    ,(format "Keybinding for `%S', as a string.
nil means no keybinding is established."
                             command-name)
                    :group 'the
                    :type 'string)
                  (when ,custom-name
                    (bind-key ,custom-name #',command-name))))))
          specs)))

  (the--establish-avy-bindings
   ("c" . avy-goto-char)
   ("t" . avy-goto-char-timer)
   ("l" . avy-goto-line)
   ("W" . avy-goto-word-1)
   ("w" . avy-goto-word-0))

  :config

  ;; Use De Bruijn sequences for jump sequences. This allows you to
  ;; fixate on a particular place you want to jump to, and just type
  ;; whatever shows up there.
  (setq avy-style 'de-bruijn))

;; This package highlights matches and previews replacements in query
;; replace.
(use-package visual-regexp
  :demand t
  :bind (;; Replace the regular query replace with the regexp query
         ;; replace provided by this package.
         ("M-%" . vr/query-replace)))

(use-package pcre2el
  :demand t
  :config
  (rxt-global-mode +1))

(defcustom the-regex-engine 'pcre2el
  "The regex engine to use for visual-regexp."
  :group 'the
  :type '(choice
          (const :tag "Emacs" emacs)
          (const :tag "pcre2el" pcre2el)
          (const :tag "Python" python)
          (const :tag "Custom" custom)))

;; This package allows the use of other regexp engines for
;; visual-regexp.
(use-package visual-regexp-steroids
  :demand t
  :after (visual-regexp pcre2el)
  :config

  ;; Use Emacs-style regular expressions by default.
  (setq vr/engine the-regex-engine))

(use-package expand-region
  :demand t)

(use-package vimish-fold
  :demand t
  :config
  (vimish-fold-global-mode 1))

(provide 'the-navigation)

;;; the-navigation.el ends here
