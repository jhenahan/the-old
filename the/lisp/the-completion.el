;;; the-completion.el --- Completion systems

(require 'the-appearance)
(require 'the-libraries)
(require 'the-package)
(require 'the-patch)

(use-package smex)

(use-package flx)

(use-package ivy
  :demand t
  :init
  (el-patch-feature ivy)
  (el-patch-defvar ivy-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map [remap switch-to-buffer]
            'ivy-switch-buffer)
          (define-key map [remap switch-to-buffer-other-window]
            'ivy-switch-buffer-other-window)
          map)
        "Keymap for `ivy-mode'.")
  (el-patch-define-minor-mode ivy-mode
    "Toggle Ivy mode on or off.
    Turn Ivy mode on if ARG is positive, off otherwise.
    Turning on Ivy mode sets `completing-read-function' to
    `ivy-completing-read'.
    Global bindings:
    \\{ivy-mode-map}
    Minibuffer bindings:
    \\{ivy-minibuffer-map}"
    :group 'ivy
    :global t
    :keymap ivy-mode-map
    :lighter " ivy"
    (if ivy-mode
        (progn
          (setq completing-read-function 'ivy-completing-read)
          (el-patch-splice 2
            (when ivy-do-completion-in-region
              (setq completion-in-region-function 'ivy-completion-in-region))))
      (setq completing-read-function 'completing-read-default)
      (setq completion-in-region-function 'completion--in-region)))
  (ivy-mode 1)
  :bind (
         ("C-x C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("<tab>" . ivy-alt-done)
         ("C-j" . ivy-immediate-done)
         )
  :config
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-flx-limit 2000)
  :delight ivy-mode)

(use-package counsel
  :bind (;; Use Counsel for common Emacs commands.
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h l" . counsel-load-library)
         ("C-h C-l" . counsel-find-library)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-x 8 RET" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-rg)
         ("M-y" . counsel-yank-pop)
         :map read-expression-map
         ("C-r" . counsel-expression-history)
         )
  :config
  (setq counsel-find-file-at-point t)
)

(use-package historian
  :demand t
  :config
  (historian-mode 1))

(use-package ivy-historian
  :demand t
  :after ivy
  :config
  (setq ivy-historian-freq-boost-factor 500)
  (setq ivy-historian-recent-boost 500)
  (setq ivy-historian-recent-decrement 50)
  (ivy-historian-mode 1))

(use-package icicles
  :demand t)

(provide 'the-completion)

;;; the-completion.el ends here
