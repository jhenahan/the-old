;;; the-org.el -- Org mode customizations

(require 'the-bind-key)
(require 'the-package)
(require 'the-libraries)
(require 'the-git)
(require 'the-modeline)

(define-globalized-minor-mode global-outline-minor-mode
  outline-minor-mode outline-minor-mode)

(global-outline-minor-mode +1)

(use-package org
  :straight org-plus-contrib
  :demand t
  :bind (
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("S-<left>" . nil)
         ("S-<right>" . nil)
         ("S-<up>" . nil)
         ("S-<down>" . nil)
         ("C-<left>" . org-shiftleft)
         ("C-<right>" . org-shiftright)
         ("C-<up>" . org-shiftup)
         ("C-<down>" . org-shiftdown)
         ([remap backward-paragraph] . org-backward-paragraph)
         ([remap forward-paragraph] . org-forward-paragraph)
         ("M-RET" . org-insert-heading)
         )
  :init
  (defun the-org-git-version ()
    (let ((git-repo
  	 (f-join user-emacs-directory "straight/repos/org")))
      (s-trim (git-run "describe"
  		     "--match=release\*"
  		     "--abbrev=6"
  		     "HEAD"))))
  (defun the-org-release ()
    (let ((git-repo
  	 (f-join user-emacs-directory "straight/repos/org")))
      (s-trim (s-chop-prefix "release_"
  			   (git-run "describe"
  				    "--match=release\*"
  				    "--abbrev=0"
  				    "HEAD")))))
  (defalias #'org-git-version #'the-org-git-version)
  (defalias #'org-release #'the-org-release)
  (provide 'org-version)
  :config
  (defun the-fix-easy-templates ()
    (require 'org-tempo))
  
  (add-hook 'org-mode-hook 'the-fix-easy-templates)
  (use-package org-bullets
    :init
    (add-hook 'org-mode-hook 'org-bullets-mode))
  (setq org-insert-heading-respect-content t)
  (add-hook 'org-mode-hook #'org-indent-mode)
  (defun the-org-sort-ignore-errors ()
    (condition-case x
        (org-sort-entries nil ?a)
      (user-error)))
  
  (defun the-org-sort-buffer ()
    "Sort all entries in the Org buffer recursively in alphabetical order."
    (interactive)
    (org-map-entries #'the-org-sort-ignore-errors))
  (defun the-org-past-entries ()
    (when (and (string= (org-get-todo-state) "DONE")
               (let ((deadline (org-entry-get (point) "DEADLINE")))
                 (or (null deadline)
                     (time-less-p (org-time-string-to-time deadline)
                                  (current-time)))))
      (org-archive-subtree)
      (setq org-map-continue-from (line-beginning-position))))
  
  
  (defun the-org-archive-past ()
    "Archive DONE items with deadlines either missing or in the past."
    (interactive)
    (org-map-entries #'the-org-past-entries))
  :delight
  (org-indent-mode)
  )

(use-package org-agenda
  :straight org-plus-contrib
  :bind (:map org-agenda-mode-map
         ("S-<up>" . nil)
         ("S-<down>" . nil)
         ("S-<left>" . nil)
         ("S-<right>" . nil)
         ("C-<left>" . org-agenda-do-date-earlier)
         ("C-<right>" . org-agenda-do-date-later)
         )
  :config
  (defun the--advice-org-agenda-split-horizontally (org-agenda &rest args)
    "Make `org-agenda' split horizontally, not vertically, by default.
    This is an `:around' advice for `org-agenda'. It commutes with
    `the--advice-org-agenda-default-directory'."
    (let ((split-height-threshold nil))
      (apply org-agenda args)))
  
  (advice-add #'org-agenda :around
              #'the--advice-org-agenda-split-horizontally)
  (defun the--advice-org-agenda-default-directory
      (org-agenda &rest args)
    "If `org-directory' exists, set `default-directory' to it in the agenda.
    This is an `:around' advice for `org-agenda'. It commutes with
    `the--advice-org-agenda-split-horizontally'."
    (let ((default-directory (if (f-exists? org-directory)
                                 org-directory
                               default-directory)))
      (apply org-agenda args)))
  
  (advice-add #'org-agenda :around
              #'the--advice-org-agenda-default-directory)
  )

(use-package htmlize)

(defun the-in-the-org-lib-p ()
  (and (f-this-file)
       (f-child-of? (f-this-file) the-org-lib-directory)))

(defun the-update-doc ()
  "Update the readme."
  (interactive)
  (save-window-excursion
    (find-file-read-only the-doc-source-file)
    (org-org-export-to-org)))


(defun the-org-lib-hook ()
  (if (the-in-the-org-lib-p)
      (progn
        (setq-local org-babel-default-header-args:emacs-lisp
                    `((:tangle . ,(f-expand (f-swap-ext (f-filename (f-this-file)) "el") the-lib-directory))
                      (:noweb . "yes")))
        (the-update-doc))))

(add-hook 'org-mode-hook 'the-org-lib-hook)

(defun the-org-lib-tangle-hook ()
  (if (the-in-the-org-lib-p)
      (org-babel-tangle)))

(add-hook 'after-save-hook 'the-org-lib-tangle-hook)

(provide 'the-org)

;;; the-org.el ends here
