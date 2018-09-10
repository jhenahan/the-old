;; -*- lexical-binding: t; -*-
;;; the-keymap.el --- Non-color-theme appearance tweaks

(require 'the-package)
(require 'the-custom)
(require 'the-bind-key)
(require 'the-hydra)
(require 'the-os)
(require 'the-libraries)
(require 'the-modeline)
(require 'the-navigation)
(require 'the-org)
(require 'the-undo)
(require 'the-network)

(use-package which-key
  :demand t
  :bind* (("H-T ?" . which-key-show-top-level))
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "H-T ?" "top level bindings")
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-enable-extended-define-key t)
  :diminish which-key-mode)

(use-package discover-my-major
  :bind (("C-h C-m" . discover-my-major)
         ("C-h H-T" . discover-my-mode)))

(the-with-operating-system macOS
  (setq ns-right-command-modifier 'none)
  (setq ns-right-option-modifier 'none)
  (setq ns-right-control-modifier 'control)
  (setq ns-control-modifier 'super)
  (setq ns-command-modifier 'meta)
  (setq ns-option-modifier 'hyper))

(defcustom the-ryo-activation t
  "If non-nil, use ryo-modal for modal editing"
  :type '(boolean)
  :group 'the)

(when the-ryo-activation
  (use-package ryo-modal
    :demand t
    :bind* (("C-z" . ryo-modal-mode))
    :config
    (bind-key "<escape>" #'ryo-modal-mode)
    (ryo-modal-keys ("<escape>" "C-g"))
    (add-hook 'text-mode-hook #'ryo-modal-mode)
    (add-hook 'prog-mode-hook #'ryo-modal-mode)
    (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)
    (setq ryo-modal-default-cursor-color "#EBDBB2")
    (setq ryo-modal-cursor-color "#FFAF00")
    :diminish "ρ"))

(defhydra the-hydra-rectangle (:pre (rectangle-mark-mode 1)
                                    :color pink
                                    :hint nil)
  "
 _p_: paste   _r_: replace  _I_: insert
 _y_: copy    _o_: open     _V_: reset
 _d_: kill    _n_: number   _q_: quit
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("y" copy-rectangle-as-kill)
  ("d" kill-rectangle)
  ("x" clear-rectangle)
  ("o" open-rectangle)
  ("p" yank-rectangle)
  ("r" string-rectangle)
  ("n" rectangle-number-lines)
  ("I" string-insert-rectangle)
  ("V" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("q" keyboard-quit :color blue))

(bind-keys*
  ("H-T V" . the-hydra-rectangle/body))

(let ((text-objects
       '(("." er/mark-symbol :name "Symbol")
         (":" er/mark-symbol-with-prefix :name "Symbol+Prefix")
         ("B" mark-whole-buffer :name "All buffer")
         ("E" org-mark-element :name "Org Element")
         ("d" er/mark-defun :name "Defun")
         ("f" avy-goto-word-1 :then (er/mark-symbol) :name "◎ Symbol")
         ("i" er/mark-inside-pairs :name "Inner")
         ("m" er/mark-method-call :name "Method call")
         ("n" er/mark-next-accessor :name "Next accessor")
         ("o" er/mark-outside-pairs :name "Outer")
         ("p" er/mark-paragraph :name "Paragraph")
         ("s" er/mark-sentence :name "Sentence")
         ("t" org-mark-subtree :name "Org Subtree")
         ("u" er/mark-url :name "URL")
         ("w" er/mark-word :name "Word")
         (";" er/mark-comment :name "Comment"))))
  (eval `(ryo-modal-keys
          ("c" ,text-objects :then '(kill-region) :exit t :name "Replace")
          ("d" ,text-objects :then '(kill-region) :name "Delete")
          ("y" ,text-objects :then '(copy-region-as-kill) :name "Yank")
          ("v" ,text-objects :name "Select")
          ("V" the-hydra-rectangle/body :name "Rectangle Select")
          ("i" ,text-objects :then '(vimish-fold) :name "Fold")
          (";" ,text-objects :then '(comment-dwim) :name "Comment")
          ("p" yank)
          ("P" yank-pop)
          ("x" delete-char)
          ("s" delete-char :exit t)
          ("o" open-line :exit t)
          ("O" open-line :first '(previous-line) :exit t)
          ("a" forward-char :exit t)
          ("A" end-of-line :exit t)
          ("i" ryo-modal-mode)
          ("I" beginning-of-line :exit t)
          ("d d" kill-whole-line)
          ("c c" kill-whole-line :exit t))))

(ryo-modal-keys
 ("," ryo-modal-repeat)
 ("RET" ryo-modal-mode)
 ("!" async-shell-command)
 ("/" swiper :name "Search")
 ("%" vr/query-replace)
 ("u" undo-tree-undo)
 ("C-r" undo-tree-redo)
 ("U" undo-tree-visualize))

 (ryo-modal-keys
  (:norepeat t)
  ("0" "M-0")
  ("1" "M-1")
  ("2" "M-2")
  ("3" "M-3")
  ("4" "M-4")
  ("5" "M-5")
  ("6" "M-6")
  ("7" "M-7")
  ("8" "M-8")
  ("9" "M-9"))

(ryo-modal-keys
 ("h" backward-char)
 ("j" next-line)
 ("k" previous-line)
 ("l" forward-char)
 ("w" forward-word)
 ("b" backward-word)
 ("{" "M-{")
 ("}" "M-}")
 ("(" backward-sentence)
 (")" forward-sentence)
 ("$" move-end-of-line)
 ("G" end-of-buffer)
 ("g g" beginning-of-buffer)
 ("g l" goto-line)
 ("+" bookmark-set)
 ("'" bookmark-jump))

(ryo-modal-keys ("SPC"
                 (("o"
                   (("a" org-agenda)
                    ("c" org-capture)
                    ("l" org-store-link)
                    ("L" org-insert-link)
                    ("C"
                     (("i" org-clock-in)
                      ("o" org-clock-out))
                     :name "Clock")
                    ("p" org-pomodoro)
                    ("RET" org-ctrl-c-ctrl-c :name "Do Something Useful")
                    ("f" org-forward-element)
                    ("b" org-backward-element))
                   :name "Org"))))

(ryo-modal-keys ("?" which-key-show-top-level))
(ryo-modal-key "SPC h"
               '(("f" "C-h f")
                 ("v" "C-h v")
                 ("k" "C-h k")
                 ("?" "C-h b")
                 ("m" "C-h C-m")
                 ("M" "C-h H-T"))
               :name "Help")

(ryo-modal-key "SPC :"
               '(("e" "C-x C-f")
                 ("r" "C-x i")))

(ryo-modal-key "SPC e"
               '(("e" eww)))

(ryo-modal-major-mode-keys
 'eww-mode
 (":" eww-browse-with-external-browser)
 ("#" eww-list-histories)
 ("{" eww-back-url)
 ("}" eww-forward-url))

(provide 'keymap)
