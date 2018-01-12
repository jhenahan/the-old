
# Table of Contents

1.  [Appearance](#orgea031b5)
    1.  [Basic Setup](#org5a6e966)
    2.  [Fullscreen](#org2443221)
    3.  [Interface Cleanup](#orgebb5fb1)
    4.  [Keystroke Display](#org3e0a865)
    5.  [No Title Bars](#orga7cf95b)
    6.  [Fonts](#org40abb4e)
    7.  [Adjust font size by screen resolution](#org5107705)
2.  [Auto-completion](#orgaa52a7c)
    1.  [Company Settings](#org424f369)
        1.  [Performance](#org33747e8)
        2.  [YaSnippet Hack](#orgdfdeb95)
    2.  [Company](#orge72131f)
    3.  [Company Statistics](#orgda39c1b)
3.  [Binding Keys](#org0335dcc)
    1.  [Custom Prefix](#org26a7b27)
    2.  [`bind-key`](#org36646dd)
4.  [Syntax Checking](#orged10095)
    1.  [Flycheck](#orgf1ea06f)
        1.  [Settings](#org8a56a93)
        2.  [`use-package` declaration](#org60a7ddb)
5.  [Clipboard Integration](#org62734e1)
    1.  [macOS integration](#org09927fc)
    2.  [Inter-program paste](#org3a704ce)
6.  [Emojis!](#orgd279293)
    1.  [`emojify`](#org66406fd)
7.  [Sensible Defaults](#org6dbf275)
    1.  [Default Directory](#org4e44e2b)
    2.  [Treat Camel-Case Words as separate words](#org8615399)
    3.  [Increase GC threshold](#org3062dd5)
    4.  [Make Scripts Executable By Default](#org46afe4a)
    5.  [Transient Mark Mode](#org6edcf5e)
    6.  [Short Confirmations](#orga740eb2)
    7.  [macOS settings](#org8e61d15)
8.  [Org Mode Customization](#orgf8bf9f5)
    1.  [Global Outline Mode](#orgaf67172)
    2.  [Org](#orgce4c31d)
        1.  [Setup](#orgeab9766)
        2.  [`use-package` declaration](#orgf94043b)
    3.  [Org Agenda](#org32695b5)
        1.  [Setup](#orgf092906)
        2.  [`use-package` declaration](#org4594f48)
    4.  [Extra Export Packages](#org81c35f3)
        1.  [`htmlize`](#org9239cba)
    5.  [Org-mode Config Settings](#orgf88a3b0)



<a id="orgea031b5"></a>

# Appearance


<a id="org5a6e966"></a>

## Basic Setup

This file has appearance tweaks that are unrelated to the color
theme. Menus, scroll bars, bells, cursors, and so on. See also
`the-theme`, which customizes the color theme specifically.


<a id="org2443221"></a>

## Fullscreen

I use `chunkwm` to manage most windows, including Emacs, so the native
fullscreen mode is unnecessary. It's also necessary to set pixelwise
frame resizing non-nil for a variety of window managers. I don't see
any particular harm in having it on, regardless of WM.

    (the-with-operating-system macOS
      (setq ns-use-native-fullscreen nil))
    (setq frame-resize-pixelwise t)


<a id="orgebb5fb1"></a>

## Interface Cleanup

Emacs defaults are a nightmare of toolbars and scrollbars and such
nonsense. We'll turn all of that off.

    (menu-bar-mode -1)
    (setq ring-bell-function #'ignore)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (blink-cursor-mode -1)


<a id="org3e0a865"></a>

## Keystroke Display

Display keystrokes in the echo area immediately, not after one
second. We can't set the delay to zero because somebody thought it
would be a good idea to have that value suppress keystroke display
entirely.

    (setq echo-keystrokes 1e-6)


<a id="orga7cf95b"></a>

## No Title Bars

I put a lot of effort into purging title bars from most of the
software I use on a regular basis (what a waste of real estate), and
in Emacs 26 (might really be 26.2 or so) this is built in. For earlier
versions, patches exist to get the same effect.

    (if (version<= "26" emacs-version)
        (setq default-frame-alist '((undecorated . t))))


<a id="org40abb4e"></a>

## Fonts

I use Pragmata Pro everywhere, but I'll eventually figure out how to
deal with fonts properly and allow this to be specified.


<a id="org5107705"></a>

## Adjust font size by screen resolution

The biggest issue I have with multiple monitors is that the font size
is all over the place. The functions below just set up some reasonable
defaults and machinery to change the size depending on the resolution
of the monitor.

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


<a id="orgaa52a7c"></a>

# Auto-completion


<a id="org424f369"></a>

## Company Settings

-   Show completions instantly, rather than after half a second.
-   Show completions after typing three characters.
-   Show a maximum of 10 suggestions. This is the default but I think
    it's best to be explicit.
-   Always display the entire suggestion list onscreen, placing it above
    the cursor if necessary.
-   Always display suggestions in the tooltip, even if there is only
    one. Also, don't display metadata in the echo area (this conflicts
    with ElDoc).
-   Show quick-reference numbers in the tooltip (select a completion
    with M-1 through M-0).
-   Prevent non-matching input (which will dismiss the completions
    menu), but only if the user interacts explicitly with Company.
-   Company appears to override our settings in `company-active-map`
    based on `company-auto-complete-chars`. Turning it off ensures we
    have full control.
-   Prevent Company completions from being lowercased in the
    completion menu. This has only been observed to happen for
    comments and strings in Clojure. (Although in general it will
    happen wherever the Dabbrev backend is invoked.)
-   Only search the current buffer to get suggestions for
    `company-dabbrev` (a backend that creates suggestions from text
    found in your buffers). This prevents Company from causing lag
    once you have a lot of buffers open.
-   Make company-dabbrev case-sensitive. Case insensitivity seems
    like a great idea, but it turns out to look really bad when you
    have domain-specific words that have particular casing.

    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 3)
    (setq company-tooltip-limit 10)
    (setq company-tooltip-minimum company-tooltip-limit)
    (setq company-show-numbers t)
    (setq company-auto-complete-chars nil)
    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-other-buffers nil)
    (setq company-dabbrev-ignore-case nil)


<a id="org33747e8"></a>

### Performance

In case autocompletion is making Emacs drag, we add a toggle to slow
it down.


    (defun the-company-toggle-slow ()
      "Slow down `company' by turning up the delays before completion starts.
    This is done in `the-slow-autocomplete-mode'."
      (if the-slow-autocomplete-mode
          (progn
            (setq-local company-idle-delay 1)
            (setq-local company-minimum-prefix-length 3))
        (kill-local-variable 'company-idle-delay)
        (kill-local-variable 'company-minimum-prefix-length)))

    (add-hook 'the-slow-autocomplete-mode-hook #'the-company-toggle-slow)


<a id="orgdfdeb95"></a>

### YaSnippet Hack

Make it so that Company's keymap overrides Yasnippet's keymap when a
snippet is active. This way, you can TAB to complete a suggestion for
the current field in a snippet, and then TAB to move to the next
field. Plus, C-g will dismiss the Company completions menu rather than
cancelling the snippet and moving the cursor while leaving the
completions menu on-screen in the same location.

    (with-eval-after-load 'yasnippet
      ;; FIXME: this is all a horrible hack, can it be done with
      ;; `bind-key' instead?
      ;;
      ;; This function translates the "event types" I get from
      ;; `map-keymap' into things that I can pass to `lookup-key'
      ;; and `define-key'. It's a hack, and I'd like to find a
      ;; built-in function that accomplishes the same thing while
      ;; taking care of any edge cases I might have missed in this
      ;; ad-hoc solution.
      (defun the-normalize-event (event)
        "This function is a complete hack, do not use.
      But in principle, it translates what we get from `map-keymap'
      into what `lookup-key' and `define-key' want."
        (if (vectorp event)
            event
          (vector event)))

      ;; Here we define a hybrid keymap that delegates first to
      ;; `company-active-map' and then to `yas-keymap'.
      (setq the-yas-company-keymap
            ;; It starts out as a copy of `yas-keymap', and then we
            ;; merge in all of the bindings from
            ;; `company-active-map'.
            (let ((keymap (copy-keymap yas-keymap)))
              (map-keymap
               (lambda (event company-cmd)
                 (let* ((event (the-normalize-event event))
                        (yas-cmd (lookup-key yas-keymap event)))
                   ;; Here we use an extended menu item with the
                   ;; `:filter' option, which allows us to
                   ;; dynamically decide which command we want to
                   ;; run when a key is pressed.
                   (define-key keymap event
                     `(menu-item
                       nil ,company-cmd :filter
                       (lambda (cmd)
                         ;; There doesn't seem to be any obvious
                         ;; function from Company to tell whether or
                         ;; not a completion is in progress (à la
                         ;; `company-explicit-action-p'), so I just
                         ;; check whether or not `company-my-keymap'
                         ;; is defined, which seems to be good
                         ;; enough.
                         (if company-my-keymap
                             ',company-cmd
                           ',yas-cmd))))))
               company-active-map)
              keymap))

      ;; The function `yas--make-control-overlay' uses the current
      ;; value of `yas-keymap' to build the Yasnippet overlay, so to
      ;; override the Yasnippet keymap we only need to dynamically
      ;; rebind `yas-keymap' for the duration of that function.
      (defun the-advice-company-overrides-yasnippet
          (yas--make-control-overlay &rest args)
        "Allow `company' to override `yasnippet'.
      This is an `:around' advice for `yas--make-control-overlay'."
        (let ((yas-keymap the-yas-company-keymap))
          (apply yas--make-control-overlay args)))

      (advice-add #'yas--make-control-overlay :around
                  #'the-advice-company-overrides-yasnippet))


<a id="orge72131f"></a>

## Company

`company` provides an in-buffer autocompletion framework. It
allows for packages to define backends that supply completion
candidates, as well as optional documentation and source code. Then
Company allows for multiple frontends to display the candidates, such
as a tooltip menu. Company stands for "Complete Anything".

    (defvar the-company-backends-global
      '(company-capf
        company-files
        (company-dabbrev-code company-keywords)
        company-dabbrev)
      "Values for `company-backends' used everywhere.
    If `company-backends' is overridden by The, then these
    backends will still be included.")

    (use-package company
      :demand t
      :config
      (company-tng-configure-default)
      <<company-config>>
      <<company-slow>>
      (global-company-mode +1)
      :delight company-mode)


<a id="orgda39c1b"></a>

## Company Statistics

`company-statistics` adds usage-based sorting to Company completions.
It is a goal to replace this package with [`historian`](https://github.com/PythonNut/historian.el) or [`prescient`](https://github.com/raxod502/prescient.el).

    (use-package company-statistics
      :demand t
      :config

      ;; Let's future-proof our patching here just in case we ever decide
      ;; to lazy-load company-statistics.
      (el-patch-feature company-statistics)

      ;; Disable the message that is normally printed when
      ;; `company-statistics' loads its statistics file from disk.
      (el-patch-defun company-statistics--load ()
        "Restore statistics."
        (load company-statistics-file 'noerror
              (el-patch-swap nil 'nomessage)
              'nosuffix))

      ;; Enable Company Statistics.
      (company-statistics-mode +1))


<a id="org0335dcc"></a>

# Binding Keys


<a id="org26a7b27"></a>

## Custom Prefix

There's a lot of room for keybindings, but we rely on a common prefix
for discoverability and to leave room for extension. This also makes
creating modal bindings later quite a bit easier.

    (defcustom the-prefix "M-T"
      "Prefix key sequence for The-related keybindings.
    This is a string as would be passed to `kbd'."
      :group 'the
      :type 'string)

For convenience, we also have a function that will create binding
strings using our prefix. This mainly gets used in bind-key
declarations until I can figure out how to evaluate code in org-table
cells to make the whole thing more customizable.

    (defun the-join-keys (&rest keys)
      "Join key sequences. Empty strings and nils are discarded.
    \(the--join-keys \"M-P e\" \"e i\") => \"M-P e e i\"
    \(the--join-keys \"M-P\" \"\" \"e i\") => \"M-P e i\""
      (string-join (remove "" (mapcar #'string-trim (remove nil keys))) " "))


<a id="org36646dd"></a>

## `bind-key`

`bind-key` is the prettier cousin of `define-key` and
`global-set-key`, as well as providing the `:bind` family of keywords
in `use-package`,

    (use-package bind-key)


<a id="orged10095"></a>

# Syntax Checking


<a id="orgf1ea06f"></a>

## Flycheck

Flycheck provides a framework for in-buffer error and warning
highlighting, or more generally syntax checking. It comes with a large
number of checkers pre-defined, and other packages define more.


<a id="org8a56a93"></a>

### Settings

1.  Enable Flycheck Globally

    Enable Flycheck in all buffers, but also allow for disabling it
    per-buffer.

        (global-flycheck-mode +1)
        (put 'flycheck-mode 'safe-local-variable #'booleanp)

2.  Disable Flycheck in the modeline

    It's honestly more distracting than anything,

        (setq flycheck-mode-line nil)


<a id="org60a7ddb"></a>

### `use-package` declaration

    (use-package flycheck
      :defer 3
      :config
      <<flycheck-global>>
      <<no-flycheck-modeline>>
      )


<a id="org62734e1"></a>

# Clipboard Integration


<a id="org09927fc"></a>

## macOS integration

Like mouse integration, clipboard integration
works properly in windowed Emacs but not in terminal Emacs (at
least by default). This code was originally based on [1](https://gist.github.com/the-kenny/267162), and then
modified based on [2](http://emacs.stackexchange.com/q/26471/12534).

    (the-with-operating-system macOS
      (the-with-terminal-emacs
        (defvar the-clipboard-last-copy nil
          "The last text that was copied to the system clipboard.
    This is used to prevent duplicate entries in the kill ring.")

        (defun the-clipboard-paste ()
          "Return the contents of the macOS clipboard, as a string."
          (let* (;; Setting `default-directory' to a directory that is
                 ;; sure to exist means that this code won't error out
                 ;; when the directory for the current buffer does not
                 ;; exist.
                 (default-directory "/")
                 ;; Command pbpaste returns the clipboard contents as a
                 ;; string.
                 (text (shell-command-to-string "pbpaste")))
            ;; If this function returns nil then the system clipboard is
            ;; ignored and the first element in the kill ring (which, if
            ;; the system clipboard has not been modified since the last
            ;; kill, will be the same). Including this `unless' clause
            ;; prevents you from getting the same text yanked the first
            ;; time you run `yank-pop'. (Of course, this is less relevant
            ;; due to `counsel-yank-pop', but still arguably the correct
            ;; behavior.)
            (unless (string= text the-clipboard-last-copy)
              text)))

        (defun the-clipboard-copy (text)
          "Set the contents of the macOS clipboard to given TEXT string."
          (let* (;; Setting `default-directory' to a directory that is
                 ;; sure to exist means that this code won't error out
                 ;; when the directory for the current buffer does not
                 ;; exist.
                 (default-directory "/")
                 ;; Setting `process-connection-type' makes Emacs use a pipe to
                 ;; communicate with pbcopy, rather than a pty (which is
                 ;; overkill).
                 (process-connection-type nil)
                 ;; The nil argument tells Emacs to discard stdout and
                 ;; stderr. Note, we aren't using `call-process' here
                 ;; because we want this command to be asynchronous.
                 ;;
                 ;; Command pbcopy writes stdin to the clipboard until it
                 ;; receives EOF.
                 (proc (start-process "pbcopy" nil "pbcopy")))
            (process-send-string proc text)
            (process-send-eof proc))
          (setq the-clipboard-last-copy text))

        (setq interprogram-paste-function #'the-clipboard-paste)
        (setq interprogram-cut-function #'the-clipboard-copy)))


<a id="org3a704ce"></a>

## Inter-program paste

If you have something on the system clipboard, and then kill something
in Emacs, then by default whatever you had on the system clipboard is
gone and there is no way to get it back. Setting the following option
makes it so that when you kill something in Emacs, whatever was
previously on the system clipboard is pushed into the kill ring. This
way, you can paste it with `yank-pop`.

    (setq save-interprogram-paste-before-kill t)


<a id="orgd279293"></a>

# Emojis!


<a id="org66406fd"></a>

## `emojify`

Emojify renders a variety of strings as emojis, as well as providing
some nice interactive functions to get emojis all over the place.

    (use-package emojify
      :init
      (add-hook 'after-init-hook #'global-emojify-mode))


<a id="org6dbf275"></a>

# Sensible Defaults


<a id="org4e44e2b"></a>

## Default Directory

When using `find-file`, search from the user's home directory.

    (setq default-directory "~/")


<a id="org8615399"></a>

## Treat Camel-Case Words as separate words

    (add-hook 'prog-mode-hook 'subword-mode)


<a id="org3062dd5"></a>

## Increase GC threshold

Allow 20MB of memory (instead of 0.76MB) before calling garbage
collection. This means GC runs less often, which speeds up some
operations.

    (setq gc-cons-threshold 20000000)


<a id="org46afe4a"></a>

## Make Scripts Executable By Default

If your file starts with a shebang, the file will be marked executable
on save.

    (add-hook 'after-save-hook
              'executable-make-buffer-file-executable-if-script-p)


<a id="org6edcf5e"></a>

## Transient Mark Mode

Transient mark means region highlighting works the way you would
expect it to coming from other editors.

    (transient-mark-mode t)


<a id="orga740eb2"></a>

## Short Confirmations

Typing out `yes` and `no` is irritating. Just use `y` or `n`.

    (fset #'yes-or-no-p #'y-or-n-p)


<a id="org8e61d15"></a>

## macOS settings

If you set Emacs as the default file handler for certain types of
files, double-clicking will open an entire new Emacs frame. This
setting causes Emacs to reuse the existing one.

    (the-with-operating-system macOS
      (setq ns-pop-up-frames nil))


<a id="orgf8bf9f5"></a>

# Org Mode Customization


<a id="orgaf67172"></a>

## Global Outline Mode

Outlines work for just about any structured text imaginable, from code
to prose. If it's got something that Emacs thinks is a paragraph, it
works. When you need a high-level overview, it's hard to beat this.

    (define-globalized-minor-mode global-outline-minor-mode
      outline-minor-mode outline-minor-mode)

    (global-outline-minor-mode +1)


<a id="orgce4c31d"></a>

## Org

Org is a hugely expansive framework (a.k.a. collection of hacks) for
organizing information, notes, tasks, calendars, and anything else
related to Org-anization.


<a id="orgeab9766"></a>

### Setup

1.  Version Hack

    Because `straight.el` runs Org directly from a Git repo, the
    autoloads Org uses to identify its version are not generated in
    the way that it expects. This causes it to either a) fail to
    determine its version at all or b) incorrectly report the version
    of the built-in Org which ships with Emacs. This causes some
    issues down the line, so we have to trick Org. This is how we do it.

    First, we have to get the Git version, here represented by a short
    hash of the current commit.

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

    Next, we need to define `org-git-version` and `org-release` eagerly.

        <<org-version>>
        <<org-release>>
        (defalias #'org-git-version #'the-org-git-version)
        (defalias #'org-release #'the-org-release)
        (provide 'org-version)

2.  `org-tempo`

    In the most recent release of Org, the way easy template expansion
    (i.e., `<s[TAB]` expands to a `begin_src` block) was changed to use
    `tempo`, so we need to require this in order to keep this very
    convenient functionality in place.

        (defun the-fix-easy-templates ()
          (require 'org-tempo))

        (add-hook 'org-mode-hook 'the-fix-easy-templates)

3.  Todo Sequence

    We use an augmented set of todo states, including TODO, IN-PROGRESS,
    WAITING, and the done states DONE and CANCELED.

        (setq org-todo-keywords
              '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

4.  Bindings

    First, we want to set up some recommended bindings as specified in the
    Org manual.

        ("C-c a" . org-agenda)
        ("C-c c" . org-capture)

    First, we move the Org bindings for `org-shift*` from the `S-` prefix
    to `C-`.

        ("S-<left>" . nil)
        ("S-<right>" . nil)
        ("S-<up>" . nil)
        ("S-<down>" . nil)
        ("C-<left>" . org-shiftleft)
        ("C-<right>" . org-shiftright)
        ("C-<up>" . org-shiftup)
        ("C-<down>" . org-shiftdown)

    By default, Org maps `org-(backward/forward)-paragraph`, but only maps
    it to the keys we overrode for shift up and down. We'll remap all
    instances so that our existing bindings for those functions will work
    as expected.

        ([remap backward-paragraph] . org-backward-paragraph)
        ([remap forward-paragraph] . org-forward-paragraph)

    Finally, we'll set up a convenient binding for inserting headings.

        ("M-RET" . org-insert-heading)

5.  Settings

    `org-insert-headline` will split your content by default, which is
    pretty dumb. We therefore set it to create a new heading, instead. We
    also activate `org-indent-mode` for more beautiful documents.

        (setq org-insert-heading-respect-content t)
        (add-hook 'org-mode-hook #'org-indent-mode)

6.  Default Org Directory

    We stick our Org files in a new directory in the home directory by
    default.

        (setq org-directory "~/org")

7.  Utilities

    1.  Recursively sort buffer entries alphabetically

            (defun the-org-sort-ignore-errors ()
              (condition-case x
                  (org-sort-entries nil ?a)
                (user-error)))

            (defun the-org-sort-buffer ()
              "Sort all entries in the Org buffer recursively in alphabetical order."
              (interactive)
              (org-map-entries #'the-org-sort-ignore-errors))

    2.  Archive dead tasks

        If tasks are marked DONE, and either have no deadline or the deadline
        has passed, archive it.

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

    3.  Pretty bullets

        We use `org-bullets` to make our outlines prettier. There's some minor
        alignment weirdness with my font, so I may need to specify the bullet
        codepoints, later.

            (use-package org-bullets
              :init
              (add-hook 'org-mode-hook 'org-bullets-mode))

    4.  Dropbox integration

        If `~/org/` doesn't exist, but `~/Dropbox/org` does, symlink the
        latter to the former.

            (if (and
                 (not (f-exists? org-directory))
                 (f-directory? "~/Dropbox/org"))
                (f-symlink "~/Dropbox/org" org-directory))


<a id="orgf94043b"></a>

### `use-package` declaration

    (use-package org
      :straight org-plus-contrib
      :bind (
             <<basic-bindings>>
             :map org-mode-map
             <<org-mode-bindings>>
             <<org-mode-remaps>>
             <<org-mode-heading>>
             )
      :init
      <<org-version-definitions>>
      <<org-dir>>
      :config
      <<org-requires>>
      <<org-bullets>>
      <<org-settings>>
      <<org-sort-buffer>>
      <<org-archive-past>>
      <<todo-states>>
      <<org-dropbox>>
      :delight
      (org-indent-mode)
      )


<a id="org32695b5"></a>

## Org Agenda

Org Agenda is for generating a more useful consolidated summary of all
or some of your tasks, according to their metadata.


<a id="orgf092906"></a>

### Setup

1.  Bindings

    Analogously to our bindings for regular org files, we'll also move
    things off of `S-` and onto `C-`.

        ("S-<up>" . nil)
        ("S-<down>" . nil)
        ("S-<left>" . nil)
        ("S-<right>" . nil)
        ("C-<left>" . org-agenda-do-date-earlier)
        ("C-<right>" . org-agenda-do-date-later)

2.  Window Splitting

    We want Org Agenda to split the window into two tall windows, rather
    than two wide windows stacked.

        (defun the--advice-org-agenda-split-horizontally (org-agenda &rest args)
          "Make `org-agenda' split horizontally, not vertically, by default.
          This is an `:around' advice for `org-agenda'. It commutes with
          `the--advice-org-agenda-default-directory'."
          (let ((split-height-threshold nil))
            (apply org-agenda args)))

        (advice-add #'org-agenda :around
                    #'the--advice-org-agenda-split-horizontally)

3.  Default Directory

    If `org-directory` exists, set `default-directory` to its value in the
    agenda so that things like `find-file` work sensibly.

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


<a id="org4594f48"></a>

### `use-package` declaration

    (use-package org-agenda
      :straight org-plus-contrib
      :bind (:map org-agenda-mode-map
             <<org-agenda-bindings>>
             )
      :config
      <<agenda-window-split>>
      <<agenda-default-directory>>
      )


<a id="org81c35f3"></a>

## Extra Export Packages

In order to correctly export Org files to certain formats, we need
some additional tools.


<a id="org9239cba"></a>

### `htmlize`

Used to convert symbols and such to HTML equivalents.

    (use-package htmlize)


<a id="orgf88a3b0"></a>

## Org-mode Config Settings

Our config files live in `the-lib-directory`, but our org source files
live in `the-org-lib-directory`. Unless I decide to start loading org
files directly (which is doable if a touch annoying, at times), for
now I want the `:tangle` attribute set for me automatically as long as
I'm working on one of THE's lib files.

Additionally, I'd like to regenerate the documentation on save so
things will always be up to date.

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

Finally, I'd like to automatically tangle the files on save.

    (defun the-org-lib-tangle-hook ()
      (if (the-in-the-org-lib-p)
          (org-babel-tangle)))

    (add-hook 'after-save-hook 'the-org-lib-tangle-hook)
