;;; the-path.el --- Setting the $PATH correctly

(require 'the-os)
(require 'the-windowed)
(require 'the-libraries)

(use-package exec-path-from-shell
  :demand t
  :config
  (the-with-operating-system macOS
    (exec-path-from-shell-initialize))
  (the-with-operating-system linux
    (exec-path-from-shell-initialize)))

(provide 'the-path)

;;; the-path.el ends here
