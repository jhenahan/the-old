;;; the-prose.el --- Useful utilities for writing prose

(require 'the-package)

(use-package flyspell
  :bind* (("M-m ] s" . flyspell-goto-next-error))
  :diminish (flyspell-mode . "φ"))

(provide 'the-prose)

;;; the-prose.el ends here
