;;; the-keymap.el --- Non-color-theme appearance tweaks

(require 'the-package)
(require 'the-custom)
(require 'the-bind-key)
(require 'the-hydra)
(require 'the-libraries)
(require 'the-modeline)

(use-package which-key
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind* (("M-T ?" . which-key-show-top-level))
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "M-T ?" "top level bindings")
  :delight (which-key-mode  ""))

(use-package discover-my-major
  :bind (("C-h C-m" . discover-my-major)
         ("C-h M-T" . discover-my-mode)))

(defcustom the-modalka-activation t
  "If non-nil, use modalka for modal editing"
  :type '(boolean)
  :group 'the)

(when the-modalka-activation
  (use-package modalka
    :demand t
    :bind* (("C-z" . modalka-mode))
    :init
    (setq modalka-cursor-type 'box)
    :config
    (bind-key "<escape>" #'modalka-mode)
    (modalka-define-kbd "<escape>" "C-g")
    (add-hook 'text-mode-hook #'modalka-mode)
    (add-hook 'prog-mode-hook #'modalka-mode)
    (setq-default cursor-type '(bar . 1))
    (setq modalka-cursor-type 'box)
    :delight (modalka-mode "µ")))

(defun the-which-key-suggestion-builder (mapping)
  (which-key-add-key-based-replacements (-second-item mapping) (-first-item mapping)))

(defun the-which-key-suggestion-generator (data)
  (-each data 'the-which-key-suggestion-builder))

(defun the-modalka-binding-builder (mapping)
  (if (s-present? (-last-item mapping))
      (modalka-define-kbd (-second-item mapping) (-last-item mapping))))

(defun the-modalka-binding-generator (data)
  (when the-modalka-activation
    (-each data 'the-modalka-binding-builder)))

(let ((data (quote (("leader key" "SPC") ("help prefix" "SPC h") ("ex key" ":") ("THE prefix" "M-T") ("movement prefix" "M-T m") ("extended prefix" "M-T :") ("send code prefix" "M-T s") ("user prefix" "M-T SPC") ("global prefix" "M-T g") ("org prefix" "M-T o") ("around" "M-T a") ("inside" "M-T i") ("prev" "M-T [") ("next" "M-T ]")))))
(the-which-key-suggestion-generator data)
)

(let ((data (quote (("org agenda" "SPC o a" "C-c a") ("org capture" "SPC o c" "C-c c") ("org store link" "SPC o l" "C-c l") ("org insert link" "SPC o L" "C-c C-l") ("org iswitchb" "SPC o b" "C-c b") ("org clock in" "SPC o C i" "C-c C-x TAB") ("org clock out" "SPC o C o" "C-c C-x C-o")))))
(the-which-key-suggestion-generator data)
(the-modalka-binding-generator data)
)

(let ((data (quote (("leave modalka" "i" "C-z") ("left" "h" "C-b") ("down" "j" "C-n") ("up" "k" "C-p") ("right" "l" "C-f") ("forward word" "w" "M-f") ("backward word" "b" "M-b") ("smart next item" "n" "M-n") ("smart prev item" "N" "M-p") ("previous paragraph/org element" "{" "M-{") ("next paragraph/org element" "}" "M-}") ("beginning of line" "0" "C-a") ("end of line" "$" "C-e") ("end of buffer" "G" "M->") ("beginning of buffer" "g g" "M-<") ("yank (vim)/kill (emacs)" "y" "M-w") ("paste (vim)/yank (emacs)" "p" "C-y") ("yank pop (paste history)" "P" "M-y") ("delete char" "x" "C-d") ("kill line" "D" "C-k") ("re-center screen" "z" "C-l") ("shell command (async)" "!" "M-&") ("scroll left" "H" "C-x <") ("scroll up" "J" "C-v") ("scroll down" "K" "M-v") ("scroll right" "L" "C-x >") ("backward sentence" "(" "M-a") ("forward sentence" ")" "M-e") ("search" "/" "C-s") ("quit (minibuffer, etc)" "E" "C-g") ("go to line" "g l" "M-g g") ("record macro" "q" "C-x (") ("end macro" "Q" "C-x )") ("set mark (visual mode)" "v" "C-SPC") ("rectangle edit mode (better visual)" "V" "M-T V") ("indent region" "=" "C-M-\\") ("set bookmark" "+" "C-x r m") ("jump to bookmark" "'" "C-x r b") ("compile/lots of other stuff" "\\\\" "C-c C-c")))))
(the-which-key-suggestion-generator data)
(the-modalka-binding-generator data)
)

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
  ("M-T V" . the-hydra-rectangle/body))

(let ((data (quote (("describe function" "SPC h f" "C-h f") ("describe variable" "SPC h v" "C-h v") ("describe key" "SPC h k" "C-h k") ("describe bindings" "SPC ?" "C-h b") ("major mode bindings" "SPC h m" "C-h C-m") ("minor mode bindings" "SPC h M" "C-h M-T")))))
(the-which-key-suggestion-generator data)
(the-modalka-binding-generator data)
)

(let ((data (quote (("ex key" ":" "") ("find file" ": e" "C-x C-f") ("insert file" ": r" "C-x i")))))
(the-which-key-suggestion-generator data)
(the-modalka-binding-generator data)
)

(provide 'keymap)
