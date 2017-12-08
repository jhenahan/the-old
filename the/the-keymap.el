;;; the-keymap.el --- Non-color-theme appearance tweaks

(require 'the-custom)
(require 'the-package)

;;; Emacs is a big beast, and which-key is the only sane way to figure
;;; it out.

(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind* (("M-m ?" . which-key-show-top-level))
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
   "M-m ?" "top level bindings"))

;;; Occasionally major modes come with a bunch of bindings, and
;;; this'll help you figure them out.

(use-package discover-my-major
  :ensure t
  :bind (("C-h C-m" . discover-my-major)
         ("C-h M-m" . discover-my-mode)))

;;; Decide whether or not we're actually using Modalka

(defcustom the-modalka-activation t
  "If non-nil, use modalka for modal editing"
  :type '(boolean)
  :group 'the)

;;;

(when the-modalka-activation
  (use-package modalka
    :ensure t
    :demand t
    :bind* (("C-z" . modalka-mode))
    :diminish (modalka-mode . "μ")
    :init
    (setq modalka-cursor-type 'box)
    :config
    (global-set-key (kbd "<escape>") #'modalka-mode)
    (modalka-global-mode 1)
    (add-to-list 'modalka-excluded-modes 'magit-status-mode)
    (add-to-list 'modalka-excluded-modes 'magit-popup-mode)
    (add-to-list 'modalka-excluded-modes 'eshell-mode)
    (add-to-list 'modalka-excluded-modes 'deft-mode)
    (add-to-list 'modalka-excluded-modes 'term-mode)
    (which-key-add-key-based-replacements
    "M-m"     "Modalka prefix"
    "M-m :"   "extended prefix"
    "M-m m"   "move prefix"
    "M-m s"   "send code prefix"
    "M-m SPC" "user prefix"
    "M-m g"   "global prefix"
    "M-m o"   "org prefix"
    "M-m a"   "expand around prefix"
    "M-m i"   "expand inside prefix"
    "M-m ["   "prev nav prefix"
    "M-m ]"   "next nav prefix")))

(provide 'the-keymap)


;;; the-keymap.el ends here
