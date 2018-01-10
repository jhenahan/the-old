;;; the-find-file.el --- Finding files

(require 'cl-lib)
(require 'the-bind-key)
(require 'the-completion)
(require 'the-custom)
(require 'the-package)
(require 'the-patch)

(defcustom the-find-dotfile-prefix
  (the-join-keys the-prefix "e")
  "Prefix key sequence for opening dotfiles.
  The function `the-register-dotfile' creates a keybinding under
  this prefix, if you ask it to."
  :group 'the
  :type 'string)


(defcustom the-find-dotfile-other-window-prefix
  (the-join-keys the-prefix "o")
  "Prefix key sequencing for opening dotfiles in another window.
The function `the-register-dotfile' creates a keybinding under
this prefix, if you ask it to.")

(defmacro the-register-dotfile
    (filename &optional keybinding pretty-filename)
  "Establish functions and keybindings to open a dotfile.
The FILENAME should be a path relative to the user's home
directory. Two interactive functions are created: one to find the
file in the current window, and one to find it in another window.
If KEYBINDING is non-nil, the first function is bound to that key
sequence after it is prefixed by `the-find-dotfile-prefix',
and the second function is bound to the same key sequence, but
prefixed instead by
`the-find-dotfile-other-window-prefix' (provided that the two
prefixes are different).
This is best demonstrated by example. Suppose FILENAME is
\".emacs.d/init.el\", KEYBINDING is \"e i\",
`the-find-dotfile-prefix' is at its default value of \"M-T
e\", and `the-find-dotfile-other-window-prefix' is at its
default value of \"M-T o\". Then `the-register-dotfile' will
create the interactive functions `the-find-init-el' and
`the-find-init-el-other-window', and it will bind them to the
key sequences \"M-T e e i\" and \"M-T o e i\" respectively.
If PRETTY-FILENAME, a string, is non-nil, then it will be used in
place of \"init-el\" in this example. Otherwise, that string will
be generated automatically from the basename of FILENAME."
  (let* ((bare-filename (replace-regexp-in-string ".*/" "" filename))
         (full-filename (concat "~/" filename))
         (defun-name (intern
                      (replace-regexp-in-string
                       "-+"
                       "-"
                       (concat
                        "the-find-"
                        (or pretty-filename
                            (replace-regexp-in-string
                             "[^a-z0-9]" "-"
                             bare-filename))))))
         (defun-other-window-name
           (intern
            (concat (symbol-name defun-name)
                    "-other-window")))
         (docstring (format "Edit file %s."
                            full-filename))
         (docstring-other-window
          (format "Edit file %s, in another window."
                  full-filename))
         (defun-form `(defun ,defun-name ()
                        ,docstring
                        (interactive)
                        (find-file ,full-filename)))
         (defun-other-window-form
           `(defun ,defun-other-window-name ()
              ,docstring-other-window
              (interactive)
              (find-file-other-window ,full-filename)))
         (full-keybinding
          (when keybinding
            (the-join-keys
             the-find-dotfile-prefix
             keybinding)))
         (full-other-window-keybinding
          (the-join-keys
           the-find-dotfile-other-window-prefix
           keybinding)))
    `(progn
       ,defun-form
       ,defun-other-window-form
       (bind-keys
        ,@(when full-keybinding
            `((,full-keybinding . ,defun-name)))
        ,@(when (and full-other-window-keybinding
                     (not (string=
                           full-keybinding
                           full-other-window-keybinding)))
            `((,full-other-window-keybinding
               . ,defun-other-window-name))))
       ;; Return the symbols for the two functions defined.
       (list ',defun-name ',defun-other-window-name))))

(the-register-dotfile ".emacs.d/init.el" "e i")
(the-register-dotfile ".emacs.d/init.local.el" "e l")

(the-register-dotfile ".gitconfig" "g c")
(the-register-dotfile ".gitexclude" "g e")
(the-register-dotfile ".gitconfig.local" "g l")

(the-register-dotfile ".config/fish/config.fish" "f c")

(setq find-file-visit-truename t)

(setq find-file-suppress-same-file-warnings t)

(setq vc-handled-backends nil)

(defun the--advice-find-file-automatically-create-directory
    (original-function filename &rest args)
  "Automatically create and delete parent directories of files.
This is an `:override' advice for `find-file' and friends. It
automatically creates the parent directory (or directories) of
the file being visited, if necessary. It also sets a buffer-local
variable so that the user will be prompted to delete the newly
created directories if they kill the buffer without saving it."
  ;; The variable `dirs-to-delete' is a list of the directories that
  ;; will be automatically created by `make-directory'. We will want
  ;; to offer to delete these directories if the user kills the buffer
  ;; without saving it.
  (let ((dirs-to-delete ()))
    ;; If the file already exists, we don't need to worry about
    ;; creating any directories.
    (unless (file-exists-p filename)
      ;; It's easy to figure out how to invoke `make-directory',
      ;; because it will automatically create all parent directories.
      ;; We just need to ask for the directory immediately containing
      ;; the file to be created.
      (let* ((dir-to-create (file-name-directory filename))
             ;; However, to find the exact set of directories that
             ;; might need to be deleted afterward, we need to iterate
             ;; upward through the directory tree until we find a
             ;; directory that already exists, starting at the
             ;; directory containing the new file.
             (current-dir dir-to-create))
        ;; If the directory containing the new file already exists,
        ;; nothing needs to be created, and therefore nothing needs to
        ;; be destroyed, either.
        (while (not (file-exists-p current-dir))
          ;; Otherwise, we'll add that directory onto the list of
          ;; directories that are going to be created.
          (push current-dir dirs-to-delete)
          ;; Now we iterate upwards one directory. The
          ;; `directory-file-name' function removes the trailing slash
          ;; of the current directory, so that it is viewed as a file,
          ;; and then the `file-name-directory' function returns the
          ;; directory component in that path (which means the parent
          ;; directory).
          (setq current-dir (file-name-directory
                             (directory-file-name current-dir))))
        ;; Only bother trying to create a directory if one does not
        ;; already exist.
        (unless (file-exists-p dir-to-create)
          ;; Make the necessary directory and its parents.
          (make-directory dir-to-create 'parents))))
    ;; Call the original `find-file', now that the directory
    ;; containing the file to found exists. We make sure to preserve
    ;; the return value, so as not to mess up any commands relying on
    ;; it.
    (prog1 (apply original-function filename args)
      ;; If there are directories we want to offer to delete later, we
      ;; have more to do.
      (when dirs-to-delete
        ;; Since we already called `find-file', we're now in the buffer
        ;; for the new file. That means we can transfer the list of
        ;; directories to possibly delete later into a buffer-local
        ;; variable. But we pushed new entries onto the beginning of
        ;; `dirs-to-delete', so now we have to reverse it (in order to
        ;; later offer to delete directories from innermost to
        ;; outermost).
        (setq-local the--dirs-to-delete (reverse dirs-to-delete))
        ;; Now we add a buffer-local hook to offer to delete those
        ;; directories when the buffer is killed, but only if it's
        ;; appropriate to do so (for instance, only if the directories
        ;; still exist and the file still doesn't exist).
        (add-hook 'kill-buffer-hook
                  #'the--kill-buffer-delete-directory-if-appropriate
                  'append 'local)
        ;; The above hook removes itself when it is run, but that will
        ;; only happen when the buffer is killed (which might never
        ;; happen). Just for cleanliness, we automatically remove it
        ;; when the buffer is saved. This hook also removes itself when
        ;; run, in addition to removing the above hook.
        (add-hook 'after-save-hook
                  #'the--remove-kill-buffer-delete-directory-hook
                  'append 'local)))))

;; Add the advice that we just defined.
(advice-add #'find-file :around
            #'the--advice-find-file-automatically-create-directory)

;; Also enable it for `find-alternate-file' (C-x C-v).
(advice-add #'find-alternate-file :around
            #'the--advice-find-file-automatically-create-directory)

;; Also enable it for `write-file' (C-x C-w).
(advice-add #'write-file :around
            #'the--advice-find-file-automatically-create-directory)

(defun the--kill-buffer-delete-directory-if-appropriate ()
  "Delete parent directories if appropriate.
This is a function for `kill-buffer-hook'. If
`the--advice-find-file-automatically-create-directory' created
the directory containing the file for the current buffer
automatically, then offer to delete it. Otherwise, do nothing.
Also clean up related hooks."
  (when (and
         ;; Stop if there aren't any directories to delete (shouldn't
         ;; happen).
         the--dirs-to-delete
         ;; Stop if `the--dirs-to-delete' somehow got set to
         ;; something other than a list (shouldn't happen).
         (listp the--dirs-to-delete)
         ;; Stop if the current buffer doesn't represent a
         ;; file (shouldn't happen).
         buffer-file-name
         ;; Stop if the buffer has been saved, so that the file
         ;; actually exists now. This might happen if the buffer were
         ;; saved without `after-save-hook' running, or if the
         ;; `find-file'-like function called was `write-file'.
         (not (file-exists-p buffer-file-name)))
    (cl-dolist (dir-to-delete the--dirs-to-delete)
      ;; Ignore any directories that no longer exist or are malformed.
      ;; We don't return immediately if there's a nonexistent
      ;; directory, because it might still be useful to offer to
      ;; delete other (parent) directories that should be deleted. But
      ;; this is an edge case.
      (when (and (stringp dir-to-delete)
                 (file-exists-p dir-to-delete))
        ;; Only delete a directory if the user is OK with it.
        (if (y-or-n-p (format "Also delete directory `%s'? "
                              ;; The `directory-file-name' function
                              ;; removes the trailing slash.
                              (directory-file-name dir-to-delete)))
            (delete-directory dir-to-delete)
          ;; If the user doesn't want to delete a directory, then they
          ;; obviously don't want to delete any of its parent
          ;; directories, either.
          (cl-return)))))
  ;; It shouldn't be necessary to remove this hook, since the buffer
  ;; is getting killed anyway, but just in case...
  (the--remove-kill-buffer-delete-directory-hook))

(defun the--remove-kill-buffer-delete-directory-hook ()
  "Clean up directory-deletion hooks, if necessary.
This is a function for `after-save-hook'. Remove
`the--kill-buffer-delete-directory-if-appropriate' from
`kill-buffer-hook', and also remove this function from
`after-save-hook'."
  (remove-hook 'kill-buffer-hook
               #'the--kill-buffer-delete-directory-if-appropriate
               'local)
  (remove-hook 'after-save-hook
               #'the--remove-kill-buffer-delete-directory-hook
               'local))

(save-place-mode 1)

(el-patch-defun save-place-alist-to-file ()
  (let ((file (expand-file-name save-place-file))
        (coding-system-for-write 'utf-8))
    (with-current-buffer (get-buffer-create " *Saved Places*")
      (delete-region (point-min) (point-max))
      (when save-place-forget-unreadable-files
        (save-place-forget-unreadable-files))
      (insert (format ";;; -*- coding: %s -*-\n"
                      (symbol-name coding-system-for-write)))
      (let ((print-length nil)
            (print-level nil))
        (pp save-place-alist (current-buffer)))
      (let ((version-control
             (cond
              ((null save-place-version-control) nil)
              ((eq 'never save-place-version-control) 'never)
              ((eq 'nospecial save-place-version-control) version-control)
              (t
               t))))
        (condition-case nil
            ;; Don't use write-file; we don't want this buffer to visit it.
            (write-region (point-min) (point-max) file
                          (el-patch-add nil 'nomsg))
          (file-error (message "Saving places: can't write %s" file)))
        (kill-buffer (current-buffer))))))

(use-package projectile
  :demand t
  :config
  (projectile-mode +1)
  (defun the-projectile-indexing-method-p (method)
    "Non-nil if METHOD is a safe value for `projectile-indexing-method'."
    (memq method '(native alien)))
  
  (put 'projectile-indexing-method 'safe-local-variable
       #'the-projectile-indexing-method-p)
)

(use-package counsel-projectile
  :straight (:host github
                   :repo "raxod502/counsel-projectile"
                   :upstream (:host github
                                    :repo "ericdanan/counsel-projectile"))
  :init
  (el-patch-feature counsel-projectile)
  (el-patch-defun counsel-projectile-commander-bindings ()
        (def-projectile-commander-method ?f
          "Find file in project."
          (counsel-projectile-find-file))
        (def-projectile-commander-method ?d
          "Find directory in project."
          (counsel-projectile-find-dir))
        (def-projectile-commander-method ?b
          "Switch to project buffer."
          (counsel-projectile-switch-to-buffer))
        (def-projectile-commander-method ?A
          (el-patch-swap
            "Search project files with ag."
            "Search project files with rg.")
          (el-patch-swap
            (counsel-projectile-ag)
            (counsel-projectile-rg)))
        (def-projectile-commander-method ?s
          "Switch project."
          (counsel-projectile-switch-project)))
  (el-patch-defun counsel-projectile-toggle (toggle)
        "Toggle Ivy version of Projectile commands."
        (if (> toggle 0)
            (progn
              (when (eq projectile-switch-project-action #'projectile-find-file)
                (setq projectile-switch-project-action
                      (el-patch-swap
                        #'counsel-projectile
                        #'counsel-projectile-find-file)))
              (define-key projectile-mode-map [remap projectile-find-file] #'counsel-projectile-find-file)
              (define-key projectile-mode-map [remap projectile-find-dir] #'counsel-projectile-find-dir)
              (define-key projectile-mode-map [remap projectile-switch-project] #'counsel-projectile-switch-project)
              (define-key projectile-mode-map [remap projectile-ag]
                (el-patch-swap #'counsel-projectile-ag #'counsel-projectile-rg))
              (define-key projectile-mode-map [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer)
              (counsel-projectile-commander-bindings))
          (progn
            (when (eq projectile-switch-project-action
                      (el-patch-swap
                        #'counsel-projectile
                        #'counsel-projectile-find-file))
              (setq projectile-switch-project-action #'projectile-find-file))
            (define-key projectile-mode-map [remap projectile-find-file] nil)
            (define-key projectile-mode-map [remap projectile-find-dir] nil)
            (define-key projectile-mode-map [remap projectile-switch-project] nil)
            (define-key projectile-mode-map (el-patch-swap
                                              [remap projectile-ag]
                                              [remap projectile-rg])
              nil)
            (define-key projectile-mode-map [remap projectile-switch-to-buffer] nil)
            (projectile-commander-bindings))))
  
      (with-eval-after-load 'projectile
        (counsel-projectile-toggle 1))
  )

(provide 'the-find-file)

;;; the-find-file.el ends here
