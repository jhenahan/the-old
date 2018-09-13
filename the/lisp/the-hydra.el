;; -*- lexical-binding: t; -*-
;;; the-hydra.el --- The many-headed keybinding machine

(require 'the-custom)
(require 'the-package)

(use-package hydra
  :demand t)

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

(provide 'the-hydra)

;;; the-hydra.el ends here
