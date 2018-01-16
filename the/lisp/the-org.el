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
         ("C-c l" . org-store-link)
         ("C-c b" . org-iswitchb)
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
  (setq org-directory "~/org")
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("g" "Groceries" entry (file+headline "~/org/groceries.org" "Groceries")
           "* %?\nEntered on %U\n  %i")
          ("w" "Work" entry (file+headline "~/org/work.org" "Tasks")
           "* TODO %?\n %i\n %a")
          ("h" "Home" entry (file+headline "~/org/home.org" "Tasks")
           "* TODO %?\n %i")))
  
          (setq org-refile-targets
                '((org-agenda-files :maxlevel . 3)))
  :config
  (defun the-fix-easy-templates ()
    (require 'org-tempo))
  
  (add-hook 'org-mode-hook 'the-fix-easy-templates)
  (use-package org-bullets
    :init
    (add-hook 'org-mode-hook 'org-bullets-mode))
  (setq org-insert-heading-respect-content t)
  (add-hook 'org-mode-hook #'org-indent-mode)
  (setq org-export-in-background t)
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
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
  (if (and
       (not (f-exists? org-directory))
       (f-directory? "~/Dropbox/org"))
      (f-symlink "~/Dropbox/org" org-directory))
  :delight
  (org-indent-mode)
  )

(use-package org-agenda
  :straight org-plus-contrib
  :demand t
  :bind (:map org-agenda-mode-map
         ("S-<up>" . nil)
         ("S-<down>" . nil)
         ("S-<left>" . nil)
         ("S-<right>" . nil)
         ("C-<left>" . org-agenda-do-date-earlier)
         ("C-<right>" . org-agenda-do-date-later)
         )
  :init
  (setq org-agenda-files '("~/org"))
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

(use-package org-context
  :demand t
  :config
  (setq org-context-capture-shortcut
        '((todo
           "t" "Todo"
           entry (file+headline place-holder "Todos")
           "* TODO %?\n OPENED: %U by %n\n FILE: %a")
          (question
           "q" "Question"
           entry (file+headline place-holder "Questions")
           "* QUESTION %?\n OPENED: %U by %n\n FILE: %a")))
  (org-context-activate))

(use-package htmlize)

(defun the-in-the-org-lib-p ()
  (and (f-this-file)
       (f-child-of? (f-this-file) the-org-lib-directory)))

(defun the-update-doc ()
  "Update the readme."
  (interactive)
  (save-window-excursion
    (progn
      (find-file the-doc-source-file)
      (org-md-export-to-markdown)
      (org-latex-export-to-pdf))))


(defun the-org-lib-hook ()
  (if (the-in-the-org-lib-p)
      (progn
        (setq-local org-babel-default-header-args:emacs-lisp
                    `((:tangle . ,(f-expand (f-swap-ext (f-filename (f-this-file)) "el") the-lib-directory))
                      (:noweb . "yes"))))))

  (add-hook 'org-mode-hook 'the-org-lib-hook)

(defun the-org-lib-tangle-hook ()
  (if (the-in-the-org-lib-p)
      (org-babel-tangle)))

(add-hook 'after-save-hook 'the-org-lib-tangle-hook)

(use-package org-tree-slide
  :config
  (org-tree-slide-presentation-profile)
  (defun the-presentation-start ()
    (text-scale-set 5)
    (setq org-confirm-babel-evaluate nil)
    (setq ns-use-native-fullscreen t)
    (toggle-frame-fullscreen))
  (defun the-presentation-stop ()
    (text-scale-set 0)
    (setq org-confirm-babel-evaluate t)
    (toggle-frame-fullscreen)
    (setq ns-use-native-fullscreen nil))
  (add-hook 'org-tree-slide-play-hook #'the-presentation-start)
  (add-hook 'org-tree-slide-stop-hook #'the-presentation-stop)
  )

(provide 'the-org)

;;; the-org.el ends here
