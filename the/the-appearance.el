;;; the-appearance.el --- Non-color-theme appearance tweaks

(require 'the-custom)
(require 'the-package)
(require 'the-windowed)
(require 'the-os)

;;; This file has appearance tweaks that are unrelated to the color
;;; theme. Menus, scroll bars, bells, cursors, and so on. See also
;;; `the-theme', which customizes the color theme specifically.

(the-with-operating-system macOS
  (setq ns-use-native-fullscreen nil))

;; Disable the menu bar.
(menu-bar-mode -1)

;; Turn off the alarm bell.
(setq ring-bell-function #'ignore)

;; Disable the scroll bars.
(scroll-bar-mode -1)

;; Disable the tool bar.
(tool-bar-mode -1)

;; Prevent the cursor from blinking.
(blink-cursor-mode -1)

;; Display keystrokes in the echo area immediately, not after one
;; second. We can't set the delay to zero because somebody thought it
;; would be a good idea to have that value suppress keystroke display
;; entirely.
(setq echo-keystrokes 1e-6)

(use-package dynamic-fonts
  :init
  (setq dynamic-fonts-preferred-monospace-fonts '("Pragmata Pro"))
  (dynamic-fonts-setup))

;; Set font size according to resolution
(defun fontify-frame (frame)
  (interactive)
  (when window-system
    (if (> (x-display-pixel-width) 2000)
        (set-frame-parameter frame 'font "PragmataPro 19") ;; Cinema Display
      (set-frame-parameter frame 'font "PragmataPro 16"))))

;; Fontify current frame
(add-hook 'after-init-hook (lambda () (fontify-frame nil)))
(add-hook 'window-setup-hook (lambda () (fontify-frame nil)))


;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)


(provide 'the-appearance)

;;; the-appearance.el ends here
