(defvar the-minimum-emacs-version "25.1"
  "THE does not support any Emacs version below this.")

(defvar the-local-init-file "~/.emacs.d/init.local.el"
  "File for local customizations of THE.")

;; Don't bother poor package.el if nothing else works
(setq package-enable-at-startup nil)

;; Stop dumping junk in my init file, Customize
(setq custom-file the-local-init-file)

(if (version< emacs-version the-minimum-emacs-version)
    (warn (concat "The Heretic's Emacs requires at least Emacs %s, "
                  "but you are running Emacs %s")
	  the-minimum-emacs-version emacs-version)
  ;; Good to go!
  (unwind-protect
      (with-demoted-errors "%S"
	(require 'cl-lib)
	(require 'subr-x)

	(defvar the-directory
	  (when-let ((link-target
		      ;; Symlink your init file, you monster
		      (file-symlink-p
		       (or
			load-file-name
			buffer-file-name))))
	    (when (file-directory-p (expand-file-name
				     "the/"
				     (file-name-directory link-target)))
	      (file-name-directory
	       (file-truename link-target))))
	  "Absolute path to THE. nil if not found.")

	(defvar the-lib-directory
	  (and the-directory
	       (expand-file-name "the/" the-directory))
	  "Absolute path to THE libs. nil if not found.")

	;; fail fast
	(unless the-directory
	  (error "Couldn't find THE."))

	;; profiles for straight.el
	(setq straight-profiles
	      '((the . "the.el")
		(the-local . "the-local.el")
		(nil . "default.el")))

	;; Use the develop branch of straight.el
        (setq straight-repository-branch "develop")

	(defvar the-after-init-hook nil
	  "Hook run after THE finishes initializing. Local init settings should likely go here.")

	;; disable local init at the command line
	(if (member "--no-local" command-line-args)
	    (progn
	      (setq command-line-args
		    (delete "--no-local" command-line-args)))

	  ;; Load local customizations. We disable eager
	  ;; macroexpansion here, since otherwise bad things can
	  ;; happen with e.g. `el-patch' as the package management
	  ;; system has not yet been loaded. See [1] for the hack
	  ;; used to disable eager macroexpansion.
	  ;;
	  ;; [1]: https://emacs.stackexchange.com/a/17329/12534
	  (cl-letf (((symbol-function #'internal-macroexpand-for-load) nil))
	    (fmakunbound 'internal-macroexpand-for-load)
	    (load the-local-init-file 'noerror 'nomessage)))

	;; make THE libs available
	(add-to-list 'load-path the-lib-directory)
	
	;; load THE libs
	(let ((preloaded-features
	       '(the-emacsd))
	      (the-features (mapcar
			     (lambda (file)
			       (intern (string-remove-suffix ".el" file)))
			     (directory-files
			      the-lib-directory nil
			      "^[a-z-]+\\.el$")))
	      ;; Any packages installed here are official THE
	      ;; packages.
	      (straight-current-profile 'the))
	  
	  ;; First we need to unload all the features, so that the
	  ;; init-file can be reloaded to pick up changes.
	  (dolist (feature the-features)
	    (setq features (remove feature features)))
	  
	  ;; Now load features that should be loaded first.
	  (dolist (feature preloaded-features)
	    (condition-case-unless-debug error-data
		(require feature)
	      (error (warn "Could not load `%S': %s" feature
			   (error-message-string error-data)))))
	  
	  ;; And then the rest of the features.
	  (dolist (feature the-features)
	    (unless (member feature preloaded-features)
	      (condition-case-unless-debug error-data
		  (require feature)
		(error (warn "Could not load `%S': %s" feature
			     (error-message-string error-data))))))
	  
	  ;; Run local customizations that are supposed to be run
	  ;; after init. Any packages installed here are
	  ;; user-local packages. (Packages installed
	  ;; interactively do not belong to either `the' or
	  ;; `the-local', and should not be written to either
	  ;; lockfile.)
	  (let ((straight-current-profile 'the-local))
	    (run-hooks 'the-after-init-hook))))))
