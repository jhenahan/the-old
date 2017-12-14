;;; the-keymap.el --- Non-color-theme appearance tweaks

(require 'the-package)
(require 'the-custom)

;;; Emacs is a big beast, and which-key is the only sane way to figure
;;; it out.

(use-package which-key
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind* (("M-m ?" . which-key-show-top-level))
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "M-m ?" "top level bindings")
  :delight (which-key-mode  ""))

;;; Occasionally major modes come with a bunch of bindings, and
;;; this'll help you figure them out.

(use-package discover-my-major
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
    :demand t
    :bind* (("C-z" . modalka-mode))
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
    (add-to-list 'modalka-excluded-modes 'help-mode)
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
      "M-m ]"   "next nav prefix")
    :diminish (modalka-mode . "µ"))


    ;; Leave Modalka
    (modalka-define-kbd "i" "C-z")

    ;;; Vim-style numeric prefix keys
    (modalka-define-kbd "0" "C-0")
    (modalka-define-kbd "1" "C-1")
    (modalka-define-kbd "2" "C-2")
    (modalka-define-kbd "3" "C-3")
    (modalka-define-kbd "4" "C-4")
    (modalka-define-kbd "5" "C-5")
    (modalka-define-kbd "6" "C-6")
    (modalka-define-kbd "7" "C-7")
    (modalka-define-kbd "8" "C-8")
    (modalka-define-kbd "9" "C-9")

    ;;; Vim-like movement (hjkl, etc.)
    (modalka-define-kbd "h" "C-b")
    (modalka-define-kbd "j" "C-n")
    (modalka-define-kbd "k" "C-p")
    (modalka-define-kbd "l" "C-f")
    (modalka-define-kbd "e" "M-f")
    (modalka-define-kbd "b" "M-b")
    (modalka-define-kbd "n" "M-n")
    (modalka-define-kbd "N" "M-p")
    (modalka-define-kbd "{" "M-{")
    (modalka-define-kbd "}" "M-}")
    (modalka-define-kbd "0" "C-a")
    (modalka-define-kbd "$" "C-e")
    (modalka-define-kbd "G" "M->")
    (modalka-define-kbd "y" "M-w")
    (modalka-define-kbd "p" "C-y")
    (modalka-define-kbd "P" "M-y")
    (modalka-define-kbd "x" "C-d")
    (modalka-define-kbd "D" "C-k")
    (modalka-define-kbd "z" "C-l")
    (modalka-define-kbd "!" "M-&")
    (modalka-define-kbd "J" "C-v")
    (modalka-define-kbd "K" "M-v")
    (modalka-define-kbd "M" "C-u")
    (modalka-define-kbd "(" "M-a")
    (modalka-define-kbd ")" "M-e")
    (modalka-define-kbd "/" "C-s")
    (modalka-define-kbd "E" "C-g")
    (modalka-define-kbd "d" "C-w")
    (modalka-define-kbd "w" "C-x o")
    (modalka-define-kbd "W" "M-m W")
    (modalka-define-kbd "B" "M-m B")
    (modalka-define-kbd "H" "C-x >")
    (modalka-define-kbd "L" "C-x <")
    (modalka-define-kbd "Z" "C-x 1")
    (modalka-define-kbd "q" "C-x (")
    (modalka-define-kbd "Q" "C-x )")
    (modalka-define-kbd "." "M-m .")
    (modalka-define-kbd "?" "M-m ?")
    (modalka-define-kbd "v" "C-SPC")
    (modalka-define-kbd "V" "M-m V")
    (modalka-define-kbd "=" "M-m =")
    (modalka-define-kbd "R" "M-m R")
    (modalka-define-kbd "X" "C-x C-x")
    (modalka-define-kbd "+" "C-x r m")
    (modalka-define-kbd "'" "C-x r b")
    (modalka-define-kbd "\\" "C-c C-c")

    ;; Global prefixed keys
    (modalka-define-kbd "g g" "M-<")
    (modalka-define-kbd "g o" "C-x C-e")
    (modalka-define-kbd "g O" "C-M-x")
    (modalka-define-kbd "g m" "M-m g m")
    (modalka-define-kbd "g M" "M-m g M")
    (modalka-define-kbd "g n" "M-m g n")
    (modalka-define-kbd "g N" "M-m g N")
    (modalka-define-kbd "g f" "M-m g f")
    (modalka-define-kbd "g F" "M-m g F")
    (modalka-define-kbd "g j" "M-m g j")
    (modalka-define-kbd "g k" "M-m g k")
    (modalka-define-kbd "g q" "M-m g q")
    (modalka-define-kbd "g w" "C-x 3")
    (modalka-define-kbd "g W" "C-x 2")
    (modalka-define-kbd "g @" "M-m g @")
    (modalka-define-kbd "g ;" "M-m g ;")
    (modalka-define-kbd "g :" "M-m g :")
    (modalka-define-kbd "g #" "M-m g #")
    (modalka-define-kbd "g {" "M-m g {")
    (modalka-define-kbd "g }" "M-m g }")
    (modalka-define-kbd "g (" "M-m g (")
    (modalka-define-kbd "g )" "M-m g )")
    (modalka-define-kbd "^" "M-m ^")
    (modalka-define-kbd "&" "M-m &")
    (modalka-define-kbd "g s" "C-x g")
    (modalka-define-kbd "g S" "C-x M-g")
    (modalka-define-kbd "g ?" "C-h k")

    ;; Region prefixed
    (modalka-define-kbd "a a" "C-x h")

    ;; Narrowing
    (modalka-define-kbd "] ]" "C-x n n")
    (modalka-define-kbd "] s" "M-m ] s")

    ;; Expanding
    (modalka-define-kbd "[ [" "C-x n w")

    ;; Quit, restart, time
    (modalka-define-kbd ": q" "C-x C-c")
    (modalka-define-kbd ": r" "C-x M-c")
    (modalka-define-kbd ": t" "M-m : t")

    ;; Prefix for common functions
    (modalka-define-kbd "g U" "C-c C-k")
    (modalka-define-kbd "SPC j" "M-x")
    (modalka-define-kbd "SPC a" "C-x b")
    (modalka-define-kbd "SPC k" "C-x k")
    (modalka-define-kbd "SPC g" "M-g g")
    (modalka-define-kbd "SPC d" "C-x d")
    (modalka-define-kbd "SPC q" "C-x 0")
    (modalka-define-kbd "SPC f" "C-x C-f")
    (modalka-define-kbd "SPC w" "C-x C-s")
    (modalka-define-kbd "SPC c" "M-m SPC c")
    (modalka-define-kbd "SPC R" "M-m SPC R")
    (modalka-define-kbd "SPC ?" "M-m SPC ?")

    ;; which-key definitions

    (which-key-add-key-based-replacements
      "0" "0"
      "1" "1"
      "2" "2"
      "3" "3"
      "4" "4"
      "5" "5"
      "6" "6"
      "7" "7"
      "8" "8"
      "9" "9")

    (which-key-add-key-based-replacements
      "ESC" "toggle mode"
      "DEL" "smart del"
      "TAB" "smart tab"
      "RET" "smart enter"
      "h"   "prev char"
      "j"   "next line"
      "k"   "prev line"
      "l"   "next char"
      "e"   "next word"
      "b"   "prev word"
      "n"   "next history item"
      "N"   "prev history item"
      "{"   "next para"
      "}"   "prev para"
      "0"   "start of line"
      "$"   "end of line"
      "("   "start of sentence"
      ")"   "end of sentence"
      "/" "search"
      "E"   "exit anything"
      "B"   "previous buffer"
      "W"   "winner undo"
      "w"   "other window"
      "G"   "end of file"
      "d"   "delete selection"
      "y"   "copy selection"
      "p"   "paste"
      "P"   "paste history"
      "x"   "delete char"
      "D"   "delete rest of line"
      "M"   "modify argument"
      "z"   "scroll center/top/bot"
      "Z"   "zoom into window"
      "H"   "scroll left"
      "J"   "scroll down"
      "K"   "scroll up"
      "L"   "scroll right"
      "'"   "org edit separately"
      "q"   "start macro"
      "Q"   "end macro"
      "?"   "top level bindings"
      "v"   "start selection"
      "R"   "overwrite mode"
      "X"   "exchange point and mark"
      "+"   "set bookmark"
      "'"   "jump to bookmark"
      "="   "indent region"
      "\\"  "C-c C-c"
      "!"   "async shell command"
      "&"   "shell command")

    (which-key-add-key-based-replacements
      "g"   "global prefix"
      "g g" "start of file"
      "g m" "make frame"
      "g M" "delete frame"
      "g n" "select frame by name"
      "g N" "name frame"
      "g j" "next pdf page"
      "g k" "previous pdf page"
      "g f" "file/url at cursor"
      "g F" "enable follow mode"
      "g o" "eval elisp"
      "g O" "eval defun"
      "g w" "vertical split win"
      "g W" "horizontal split win"
      "g S" "split line"
      "g @" "compose mail"
      "g #" "list eww histories"
      "g x" "browse with eww"
      "g :" "browse with external browser"
      "g {" "eww back"
      "g }" "eww forward"
      "g (" "info previous"
      "g )" "info next"
      "^"   "info up"
      "&"   "info goto"
      "g q" "format para"
      "g ?" "find command bound to key")

    (which-key-add-key-based-replacements
      "i"   "exit mode"
      "a" "expand entire buffer")

    (which-key-add-key-based-replacements
      "]"   "forward nav/edit"
      "] ]" "narrow region"
      "] s" "next spell error")

    (which-key-add-key-based-replacements
      "["   "backward nav/edit"
      "[ [" "widen region")

    (which-key-add-key-based-replacements
      ":"   "extended prefix"
      ": q" "quit emacs"
      ": r" "restart emacs"
      ": t" "initiliazation time")

    (which-key-add-key-based-replacements
      "SPC"   "custom prefix"
      "SPC ?" "describe bindings"
      "SPC j" "jump to cmd"
      "SPC f" "find file"
      "SPC a" "switch buffers"
      "SPC g" "goto line"
      "SPC d" "dired"
      "SPC k" "close buffer"
      "SPC w" "save buffer"
      "SPC c" "load theme"
      "SPC R" "locate"
      "SPC q" "quit window"
      "g U"   "simulate C-c C-k")
    )


(provide 'the-keymap)


;;; the-keymap.el ends here
