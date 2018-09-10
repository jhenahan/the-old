;; -*- lexical-binding: t; -*-
;;; the-appearance.el --- Non-color-theme appearance tweaks

(require 'the-custom)
(require 'the-package)
(require 'the-windowed)
(require 'the-os)

(the-with-operating-system macOS
  (setq ns-use-native-fullscreen nil))
(setq frame-resize-pixelwise t)

(menu-bar-mode -1)
(setq ring-bell-function #'ignore)
(scroll-bar-mode -1)
(defun the-disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'the-disable-scroll-bars)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(setq echo-keystrokes 1e-6)

(if (version<= "26" emacs-version)
    (progn
      (setq frame-resize-pixelwise t)
      (setq default-frame-scroll-bars 'none)
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . dark))
                                        ;(setq default-frame-alist '((undecorated . t)))
      ))

(defun the-fontify-frame (frame)
  (interactive)
  (the-with-windowed-emacs
    (if (> (x-display-pixel-width) 2000)
        (set-frame-parameter frame 'font "PragmataPro 22") ;; Cinema Display
      (set-frame-parameter frame 'font "PragmataPro 16"))))

(defun the-fontify-this-frame ()
  (interactive)
  (the-fontify-frame nil))

(defun the-fontify-idle ()
  (interactive)
  (the-fontify-this-frame)
  (run-with-idle-timer 1 t 'the-fontify-this-frame))

(call-interactively 'the-fontify-idle)

(defcustom the-fancy-stuff nil
  "Non-nil means turn on a bunch of weird appearance tweaks like
transparency."
  :group 'the
  :type 'boolean)

(if the-fancy-stuff
    (add-to-list 'default-frame-alist '(alpha . (85 . 50))))

(provide 'the-appearance)

;;; the-appearance.el ends here
