;;; the-git.el --- Interfacing with Git.

(require 'the-appearance)
(require 'the-package)
(require 'the-patch)

(use-package git
  :demand t)

(use-package magit
  :bind (;; Add important keybindings for Magit as described in the
	 ;; manual [1].
	 ;;
	 ;; [1]: https://magit.vc/manual/magit.html#Getting-Started
	 ("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup))
  :init

  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))

  :config

  ;; Enable the C-c M-g shortcut to go to a popup of Magit commands
  ;; relevant to the current file.
  (global-magit-file-mode +1)

  ;; The default location for git-credential-cache is in
  ;; ~/.config/git/credential. However, if ~/.git-credential-cache/
  ;; exists, then it is used instead. Magit seems to be hardcoded to
  ;; use the latter, so here we override it to have more correct
  ;; behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (let* ((xdg-config-home (or (getenv "XDG_CONFIG_HOME")
				(expand-file-name "~/.config/")))
	   (socket (expand-file-name "git/credential/socket" xdg-config-home)))
      (setq magit-credential-cache-daemon-socket socket))))

;; Allows editing Git commit messages from the command line (i.e. with
;; emacs or emacsclient as your core.editor).
(use-package git-commit
  :init

  ;; Lazy-load `git-commit'.

  (el-patch-feature git-commit)

  (el-patch-defconst git-commit-filename-regexp "/\\(\
     \\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
     \\|BRANCH_DESCRIPTION\\)\\'")

  (el-patch-defun git-commit-setup-check-buffer ()
    (and buffer-file-name
	 (string-match-p git-commit-filename-regexp buffer-file-name)
	 (git-commit-setup)))

  (el-patch-define-minor-mode global-git-commit-mode
    "Edit Git commit messages.
     This global mode arranges for `git-commit-setup' to be called
     when a Git commit message file is opened.  That usually happens
     when Git uses the Emacsclient as $GIT_EDITOR to have the user
     provide such a commit message."
    :group 'git-commit
    :type 'boolean
    :global t
    :init-value t
    :initialize (lambda (symbol exp)
		  (custom-initialize-default symbol exp)
		  (when global-git-commit-mode
		    (add-hook 'find-file-hook 'git-commit-setup-check-buffer)))
    (if global-git-commit-mode
	(add-hook  'find-file-hook 'git-commit-setup-check-buffer)
      (remove-hook 'find-file-hook 'git-commit-setup-check-buffer)))

  (global-git-commit-mode 1)

  :config

  ;; Wrap summary at 50 characters as per [1].
  ;;
  ;; [1]: http://chris.beams.io/posts/git-commit/
  (setq git-commit-summary-max-length 50))

(use-package magithub
  :demand t
  :after magit
  :config (magithub-feature-autoinject t))

(use-package git-link
  :demand t
  :bind (("C-c C-g l" . git-link)))

(provide 'the-git)

;;; the-git.el ends here
