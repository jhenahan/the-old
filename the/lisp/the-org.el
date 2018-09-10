;; -*- lexical-binding: t; -*-
;;; the-org.el -- Org mode customizations

(require 'the-bind-key)
(require 'the-package)
(require 'the-libraries)
(require 'the-git)
(require 'the-modeline)
(require 'the-crypt)

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
         ("C-c b" . org-switchb)
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
         ("M-<return>" . org-insert-heading)
         ("M-S-RET" . org-insert-todo-heading)
         ("M-S-<return>" . org-insert-todo-heading)
         )
  :config
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
           "* BACKLOG %?\n %T\n  %i\n  %a")
          ("m" "Meeting" entry (file+headline "~/org/work.org" "Meetings")
           "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
          ("p" "Phone call" entry (file+headline "~/org/work.org" "Phone Calls")
           "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
          ("g" "Groceries" entry (file+headline "~/org/groceries.org" "Groceries")
           "* %?\nEntered on %U\n  %i")
          ("w" "Work" entry (file+headline "~/org/work.org" "Tasks")
           "* TODO %?\n %T\n %i\n %a")
          ("h" "Home" entry (file+headline "~/org/home.org" "Tasks")
           "* TODO %?\n %i")))
  
          (setq org-refile-targets
                '((org-agenda-files :maxlevel . 3)))
  (use-package org-bullets
    :init
    (add-hook 'org-mode-hook 'org-bullets-mode))
  (setq org-insert-heading-respect-content t)
  (add-hook 'org-mode-hook #'org-indent-mode)
  (setq org-export-in-background t)
  (setq org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil)
  (setq org-log-into-drawer t)
  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t)
  (setq org-return-follows-link t)
  ;(add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
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
        '((sequence
           "BACKLOG(b!)"
           "TODO(t!)"
           "NEXT(n)"
           "IN-PROGRESS(i!)"
           "|"
           "DONE(d!)")
          (sequence
           "WAITING(w@/!)"
           "HOLD(h@/!)"
           "|"
           "CANCELED(c@)")
          (type
           "PHONE(p!)"
           "MEETING(m!)")))
  
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("IN-PROGRESS" :foreground "red" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))
  
  (setq org-todo-state-tags-triggers
        (quote (("CANCELED" ("CANCELED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-archive-location (f-expand "archive/%s::* Archived Tasks" org-directory))
  (if (and
       (not (f-exists? org-directory))
       (f-directory? "~/Dropbox/org"))
      (f-symlink "~/Dropbox/org" org-directory))
  (setq org-tag-persistent-alist
        '((:startgroup . nil)
          ("@work" . ?w)
          ("@home" . ?h)
          ("@phone" . ?p)
          ("@mail" . ?m)
          (:endgroup . nil)
          ("ansible" . ?a)
          ("epic" . ?e)
          ("linux" . ?l)
          ("noexport" . ?n)
          ("crypt" . ?c)
          ))
  (with-eval-after-load 'org-src
    (diminish 'org-src-mode))
  (with-eval-after-load 'org-indent
    (diminish 'org-indent-mode))
  :delight
  (org-mode "Ο")
  )

(use-package org-agenda
  :straight org-plus-contrib
  :after (org)
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
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t)
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
  :delight
  (org-agenda-mode "Οα ")
  )

(use-package org-clock
  :straight org-plus-contrib
  :after (org-agenda)
  :bind (:map org-mode-map
         ("C-c C-x C-i" . org-clock-in)
         ("C-c C-x C-o" . org-clock-out))
  :demand t)

(use-package org-pomodoro
  :after (org-clock)
  :bind
  ("M-T c p" . org-pomodoro)
  :demand t)

(use-package org-crypt
  :straight org-plus-contrib
  :demand t
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key "17F07DF3086C4BBFA5799F38EF21DED4826AAFCF"))

(use-package org-journal
  :demand t
  :config
  (setq org-journal-dir (f-expand "journal" org-directory))
  (setq org-journal-enable-encryption t))

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
    (disable-theme 'gruvbox)
    (load-theme 'leuven)
    (toggle-frame-fullscreen))
  (defun the-presentation-stop ()
    (text-scale-set 0)
    (setq org-confirm-babel-evaluate t)
    (disable-theme 'leuven)
    (load-theme 'gruvbox)
    (setq ns-use-native-fullscreen nil))
  (add-hook 'org-tree-slide-play-hook #'the-presentation-start)
  (add-hook 'org-tree-slide-stop-hook #'the-presentation-stop)
  )

(provide 'the-org)

;;; the-org.el ends here
