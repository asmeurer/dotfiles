;; Emacs config file

;; ====== TODO ======
;;
;; - Highlight 'single quoted' text and "double quoted" text differently in
;;   Python.
;; - Get some kind of flymake working for LaTeX.
;; - Smarter completion for Python.
;; - Export all face changes to the theme.
;; - Fix TeXcount.
;; - Fix bug with isearch when the search is not found and you type delete.
;; - Make isearch always wrap around on the first search.
;; - Find a better regex solution (icicles maybe).
;; - Fix mouse scrolling.
;; - Fix inconsistency between C-x # and C-x C-c in emacsclient.
;; - Make C-SPC unset the mark when selection is active.
;; - Find a way to let the cursor move off screen.
;; - Make newlines not indent if the line already has text

;; Thanks to http://homepages.inf.ed.ac.uk/s0243221/emacs/ for many of these

;; Don't show the splash screen on startup.  Aside from me not needing it
;; anymore, this fixes a bug with emacsclient.

(setq inhibit-splash-screen t)

;; Use better indentation for C files

(setq c-default-style "cc-mode"
      c-basic-offset 4)

;;
;; TeXcount setup for TeXcount version 2.3
;;
(defun texcount-setup ()
  (defun latex-word-count ()
    (interactive)
    (let*
        ( (this-file (buffer-file-name))
          (enc-str (symbol-name buffer-file-coding-system))
          (enc-opt
           (cond
            ((string-match "utf-8" enc-str) "-utf8")
            ((string-match "latin" enc-str) "-latin1")
            ("-encoding=guess")
            ) )
          (word-count
           (with-output-to-string
             (with-current-buffer standard-output
               (call-process "texcount" nil t nil "-0" enc-opt this-file)
               ) ) ) )
      (message word-count)
      ) )
  (define-key 'latex-mode-map "\C-c w" 'latex-word-count)
  )
(add-hook 'latex-mode-hook 'latex-setup t)

;; Commands to interact with the Mac OS X clipboard

(defun osx-copy (beg end)
  (interactive "r")
  (call-process-region beg end  "pbcopy"))

(defun osx-paste (beg end)
  (interactive "r")
  (if (region-active-p) (delete-region beg end) nil)
  (call-process "pbpaste" nil t nil))

(define-key global-map (kbd "C-x C-w") 'osx-copy)
(define-key global-map (kbd "C-x C-y") 'osx-paste)

;; ==== Make DEL delete four spaces at the beginning of a line ====

;; (defun remove-indentation-spaces ()
;;   "remove TAB-WIDTH spaces from the beginning of this line"
;;   (interactive)
;;   (if (save-excursion (re-search-backward "[^ \t]" (line-beginning-position) t))
;;       (delete-backward-char 1)
;;     (indent-rigidly (line-beginning-position) (line-end-position) (- tab-width))))
;;
;; (global-set-key (kbd "DEL") 'remove-indentation-spaces)

;; ==== Automatically update table of contents in rst-mode ====

(add-hook 'rst-adjust-hook 'rst-toc-update)

;; ==== Useful tools for removing duplicate lines ====

(defun remove-duplicate-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun remove-duplicate-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (remove-duplicate-lines-region (point-min) (point-max)))

;; ==== Fix Shift-Up to do selection ====

;; See http://lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00211.html

(define-key input-decode-map "\e[1;2A" [S-up])

(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map "\e[1;2A" [S-up]))

;; ==== Put autosave and backup files in ~/.emacs.d ====

;; Thanks to
;; http://stackoverflow.com/questions/2020941/emacs-newbie-how-can-i-hide-the-buffer-files-that-emacs-creates
;;
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.

(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosave/" t)
(make-directory "~/.emacs.d/backup/" t)

;; ==== Save command histories ====

;; See
;; http://stackoverflow.com/questions/1229142/how-can-i-save-my-mini-buffer-history-in-emacs
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))

(setq savehist-file "~/.emacs.d/savehist")

(savehist-mode 1)

;; ==== Set F7 to delete whole line ====

;; iTerm2 doesn't send C-S-Backspace to emacs, so set it up as a keyboard
;; shortcut to F7 in the iTerm2 prefs. See
;; http://code.google.com/p/iterm2/issues/detail?id=1702.

;; TODO: kill-whole-line kills a newline too, which I don't like. So write a
;; custom routine.

(defun kill-total-line ()
    (interactive)
    (let ((kill-whole-line t))
      (end-of-line)
      (kill-line 0)
      )
    )

(global-set-key [f7] 'kill-total-line)

;; define the function to kill the characters from the cursor
;; to the beginning of the current line

(defun backward-kill-line (arg)
    "Kill chars backward until encountering the beginning of a line.  If the
cursor is already at the beginning, delete the newline.  Acts like the reverse
    of kill-line (C-k)."
      (interactive "p")
        (kill-line 0))

;; I don't use C-u's normal use, but I do use this macro.

(defun backward-kill-line-or-newline ()
  "Same as backward-kill-line except if the cursor is at the beginning of the
  line, kill the character after the cursor.  Effectively the reverse of
  kill-line (C-k)"
  (interactive)
  (if (equal (point) (line-beginning-position))
      (backward-delete-char 1) (backward-kill-line 1)))

;; Furthermore, I want C-k and C-u to kill a region if it exists

;; See http://stackoverflow.com/a/8956311/161801

(defun kill-line-or-region ()
  "kill region if active only or kill line normally"
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'kill-line)))

(global-set-key (kbd "C-k") 'kill-line-or-region)

(defun backward-kill-line-or-region ()
  "kill region if active only or kill line normally"
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'backward-kill-line-or-newline)))

(global-set-key "\C-u" 'backward-kill-line-or-region)

;; You can still get the original meaning of C-u (universal-argument) with C-c
;; u.  Note, I was going to do C-S-u, but apparently terminals can't
;; distinguish the shift with control.  But see below for a workaround.

(global-set-key (kbd "C-c u") 'universal-argument)

;; I have iTerm 2 set to make Shift-Control-U send f8.
(global-set-key [f8] 'universal-argument)

;; ==== Switch to new buffer on C-x 2 or C-x 3 ====
;; Thanks to http://stackoverflow.com/a/6465599/161801

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

;; ===== Set M-Spc to also delete newlines =====
;; Requires Emacs 24 to work

;; (defun remove-indentation-spaces ()
;;   "remove TAB-WIDTH spaces from the beginning of this line"
;;   (interactive)
;;   (if (save-excursion (re-search-backward "[^ \t]" (line-beginning-position) t))
;;       (delete-backward-char 1)
;;     (indent-rigidly (line-beginning-position) (line-end-position) (- tab-width))))
;;

(defun just-one-space-with-newline ()
  "Call just-one-space with a negative argument"
  (interactive)
  (just-one-space -1))

(global-set-key (kbd "M-SPC") 'just-one-space-with-newline)

;; ===== Set C-x C-c to do the right thing in emacsclient
;; TODO

;; ===== Make the *scratch* buffer use text mode by default ====

(setq initial-major-mode 'text-mode)

;; ========== Add a directory to the emacs load-path for extensions =========

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; ===== Ask for y/n instead of yes/no =====
;; Thanks to http://asmeurersympy.wordpress.com/2012/07/09/emacs-7-months-later/#comment-584
(fset 'yes-or-no-p 'y-or-n-p)

;; ========== Line by line scrolling ==========

;; This makes the buffer scroll by only a single line when the up or
;; down cursor keys push the cursor (tool-bar-mode) outside the
;; buffer. The standard emacs behavior is to re-position the cursor in
;; the center of the screen, but this can make the scrolling confusing

(setq scroll-step 1)

;; ========== Enable Line and Column Numbering ==========

;; Show line-number in the mode line

(line-number-mode 1)

;; Show column-number in the mode line

(column-number-mode 1)

;; Disable all the version control stuff
;; Makes emacs load much faster inside git repos

(setq vc-handled-backends nil)

;; ========== Set the fill column ==========

(setq-default fill-column 78)

;; ======== Unfill commands (opposite of M-q) =====
;; Taken from http://ergoemacs.org/emacs/emacs_unfill-paragraph.html

;; This works by using fill-paragraph on a really large fill-column. It may
;; not work for *really* long paragraphs.

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the reverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the reverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

;; ======== Save undo history to file =======
;; Taken from
;; http://stackoverflow.com/questions/2985050/is-there-any-way-to-have-emacs-save-your-undo-history-between-sessions

;; Currently commented out because it saves junk files in the same directory
;; as the file being edited, and it doesn't even work.

(defun save-undo-filename (orig-name)
  "given a filename return the file name in which to save the undo list"
  (concat (file-name-directory orig-name)
          "."
          (file-name-nondirectory orig-name)
          ".undo"))

(defun save-undo-list ()
  "Save the undo list to a file"
  (save-excursion
    (ignore-errors
      (let ((undo-to-save `(setq buffer-undo-list
                                 ',buffer-undo-list))
            (undo-file-name (save-undo-filename
                             (buffer-file-name))))
        (find-file undo-file-name)
        (erase-buffer)
        (let (print-level
              print-length)
          (print undo-to-save
                 (current-buffer)))
        (let ((write-file-hooks
               (remove
                'save-undo-list
                write-file-hooks)))
          (save-buffer))
        (kill-buffer))))
  nil)

(defvar handling-undo-saving nil)

(defun load-undo-list ()
  "load the undo list if appropriate"
  (ignore-errors
    (when (and
           (not handling-undo-saving)
           (null buffer-undo-list)
           (file-exists-p
            (save-undo-filename
             (buffer-file-name))))
      (let* ((handling-undo-saving t)
             (undo-buffer-to-eval
              (find-file-noselect
               (save-undo-filename
                (buffer-file-name)))))
        (eval (read undo-buffer-to-eval))))))

;; (add-hook 'write-file-hooks 'save-undo-list)
;; (add-hook 'find-file-hook 'load-undo-list)

;; ===== Turn on Auto Fill mode automatically in all modes =====

;; Auto-fill-mode the the automatic wrapping of lines and insertion of
;; newlines when the cursor goes over the column limit.

;; This should actually turn on auto-fill-mode by default in all major
;; modes. The other way to do this is to turn on the fill for specific modes
;; via hooks.

;; TODO: Turn this on only for text modes and similar

(setq auto-fill-mode 1)

;; ===== Make Text mode the default mode for new buffers =====

(setq default-major-mode 'text-mode)

;; ===== ido mode =====

(require 'ido)
;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable
                                           'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; sort ido filelist by mtime instead of alphabetically
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (time-less-p
                 (sixth
                  (file-attributes (concat ido-current-directory b)))
                 (sixth
                  (file-attributes (concat ido-current-directory a)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (and (char-equal
                                (string-to-char x) ?.) x))
              ido-temp-list))))

;; ===== Turn on flyspell-mode ====

;; Use the turn-on-flyspell one to enable it everywhere, and the
;; flyspell-prog-mode to enable it only in comments/strings
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode) ; All programming languages
(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))

;; ===== Turn on flymake-mode ====

(add-to-list 'load-path "~/Documents/flymake-easy")

(add-hook 'c-mode-common-hook 'turn-on-flymake)
(add-hook 'latex-mode-hook 'turn-on-flymake)
(add-hook 'LaTeX-mode-hook 'turn-on-flymake)
;(add-hook 'python-mode-hook 'turn-on-flymake)
(defun turn-on-flymake ()
  "Force flymake-mode on. For use in hooks."
  (interactive)
  (flymake-mode 1))

(add-hook 'c-mode-common-hook 'flymake-keyboard-shortcuts)
(add-hook 'latex-mode-hook 'flymake-keyboard-shortcuts)
(add-hook 'LaTeX-mode-hook 'flymake-keyboard-shortcuts)
(add-hook 'python-mode-hook 'flymake-keyboard-shortcuts)
(defun flymake-keyboard-shortcuts ()
  "Add keyboard shortcuts for flymake goto next/prev error."
  (interactive)
  (local-set-key "\M-n" 'flymake-goto-next-error)
  (local-set-key "\M-p" 'flymake-goto-prev-error))

(add-to-list 'load-path "~/Documents/flymake-python-pyflakes")
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;; ===== Automatically indent with RET =====

(define-key global-map (kbd "RET") 'newline-and-indent)

;; ===== Clear trailing whitespace on save ====

;; This trick makes C-x C-s clear whitespace, *but*, if there was whitespace
;; between the cursor and the end of the line, it puts it back after the save
;; (but still marks the file as saved).  This way, you can save while typing
;; and it won't move the cursor back into the word you just typed if you had a
;; space there, but it always clears whitespace in the saved file.
;; Additionally, this makes C-x C-s clear whitespace even if no changes have
;; been made to the file yet.  To save a file without clearing whitespace, use
;; M-x save-buffer.

(defun my-save-buffer-dtws (arg)
  "save buffer delete trailing white space, preserve white space before
    point if point is past text"
  (interactive "p")
  (let ((save (when (and (looking-at "\\s-*$")
                         (looking-back "\\s-+"
                                       (line-beginning-position) t))
                (match-string 0))))
    (delete-trailing-whitespace)
    (save-buffer arg)
    (when save
      (insert save)
      (set-buffer-modified-p nil))))

(global-set-key [remap save-buffer] 'my-save-buffer-dtws)

;; Normally you would just do this:

;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ===== Use four spaces instead of tabs ====

(setq c-basic-offset 4)
(setq tab-width 4)
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq indent-line-function 'insert-tab)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76
                        80))
(setq-default py-indent-offset 4)
(setq-default python-indent 4)

;; ===== Trailing whitespace ======

(setq-default highlight-trailing-whitespace)

;; ===== Highlight Marked Text =====

(setq-default transient-mark-mode t)

;; ===== Abbreviations =====

(define-abbrev global-abbrev-table "Ondrej" "Ondřej")
(define-abbrev global-abbrev-table "Certik" "Čertík")

;; ===== Things for C mode ====

;; Automatically add newlines when typing braces and such
(setq c-auto-newline t)
;; Make delete undo it all at once
(setq c-hungry-delete-key t)

;; ===== Flymake for tex-mode ====

;; flymake-mode for tex uses texify by default, which only works in Windows (miktex)

;; Borrowed from https://github.com/MassimoLauria/dotemacs/blob/master/init-latex.el
(defun init-latex--flymake-setup ()
  "Setup flymake for latex using one of the checker available on the system.
It either tries \"lacheck\" or \"chktex\"."
  (interactive)
  (cond ((executable-find "lacheck")
         (defun flymake-get-tex-args (file-name)
           (list "lacheck" (list file-name))))
        ((executable-find "chktex")
         (defun flymake-get-tex-args (file-name)
           (list "chktex" (list "-q" "-v0" file-name))))
        (t nil)))

(eval-after-load "flymake" '(init-latex--flymake-setup))

(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)

;; ===== Enable auto-fill-mode for relevant file types =====

(defun turn-on-auto-fill ()
  "Force auto-fill-mode on. For us in hooks."
  (interactive)
  (auto-fill-mode 1))

(add-hook 'message-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill) ; All programming languages
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; ===== Enable mouse support (?) ====

(require 'xt-mouse)
(xterm-mouse-mode)
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))

;; Scrolling.
;; Note that this is Mac OS X style "upside down" scrolling.

;; For some reason, a single scroll reports two <mouse-4> or <mouse-5> events,
;; so we must use this hack to scroll by one line at a time

;; TODO: (interactive "@") causes scrolling to follow the mouse, but it
;; switches buffers.  Figure out how to make it not switch.  See http://stackoverflow.com/questions/11532149/emacs-make-custom-scrolling-function-follow-mouse-but-not-change-keyboard-focus.

(setq mouse-wheel-follow-mouse 't)

(defvar alternating-scroll-down-next t)
(defvar alternating-scroll-up-next t)

(defun alternating-scroll-down-line ()
  (interactive "@")
    (when alternating-scroll-down-next
          (scroll-down-line))
      (setq alternating-scroll-down-next (not alternating-scroll-down-next)))

(defun alternating-scroll-up-line ()
  (interactive "@")
    (when alternating-scroll-up-next
          (scroll-up-line))
      (setq alternating-scroll-up-next (not alternating-scroll-up-next)))

(global-set-key (kbd "<mouse-4>") 'alternating-scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'alternating-scroll-up-line)

;; ;; Try to make Python's auto-indent of line continuations smarter
;; ;; Taken from
;; ;; http://stackoverflow.com/questions/4293074/in-emacs-python-mode-customize-multi-line-statement-indentation.
;;
;; (defadvice python-calculate-indentation (around ignore-parens activate)
;;   "Indent lines inside parentheses like backslash-continued lines."
;;   (let ((syntax (save-excursion (beginning-of-line) (syntax-ppss))))
;;     (if (or (eq 'string (syntax-ppss-context syntax))
;;             (not (python-continuation-line-p))
;;             (not (cadr syntax)))
;;         ad-do-it (setq ad-return-value
;;                        (save-excursion
;;                          (beginning-of-line)
;;                          (forward-line -1)
;;                          (if (python-continuation-line-p)
;;                              (current-indentation)
;;                            (python-beginning-of-statement)
;;                            (+ (current-indentation) python-continuation-offset
;;                               (if (python-open-block-statement-p t)
;;                                   python-indent
;;                                 0))))))))
;;
;; (ad-activate 'python-calculate-indentation)

;; ===== Disable certain annoying beeps =====

;; See
;; http://stackoverflow.com/questions/11679700/emacs-disable-beep-when-trying-to-move-beyond-the-end-of-the-document/11679758#11679758

(defun my-bell-function ()
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                                keyboard-quit mwheel-scroll down up next-line
                                previous-line backward-char forward-char
                                alternating-scroll-up-line alternating-scroll-down-line))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;; ===== Highlight tabs ====
(standard-display-ascii ?\t ">>>|")
(add-hook 'text-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\t" 0 'trailing-whitespace prepend)))))

;; ===== Extensions stuff =======
;; ==============================

;; ==== ace jump mode ======

(add-to-list 'load-path "~/Documents/ace-jump-mode")
(autoload
    'ace-jump-mode
      "ace-jump-mode"
        "Emacs quick move minor mode"
          t)

(define-key global-map (kbd "C-x SPC") 'ace-jump-mode)
(setq ace-jump-mode-case-fold t)

;; enable a more powerful jump back function from ace jump mode
;; Commented out because it doesn't work if you haven't been using ace

;; (autoload
;;     'ace-jump-mode-pop-mark
;;       "ace-jump-mode"
;;         "Ace jump back:-)"
;;           t)
;; (eval-after-load "ace-jump-mode"
;;     '(ace-jump-mode-enable-mark-sync))
;; (define-key global-map (kbd "C-x C-x") 'ace-jump-mode-pop-mark)

;; ==== smex (ido for M-x) ======

(add-to-list 'load-path "~/Documents/smex") ;; The git clone
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x M-x") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; ==== ido-ubiquitous =====
;; ==== Gives ido mode really everywhere =====

(add-to-list 'load-path "~/Documents/ido-ubiquitous")
(require 'ido-ubiquitous)
(ido-ubiquitous)

;; ==== Markdown mode =====

(add-to-list 'load-path "~/Documents/markdown-mode") ;; The git clone

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown
files" t)
(autoload 'gfm-mode "markdown-mode.el" "Major mode for editing GitHub flavored
Markdown" t)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("PULLREQ_EDITMSG" . gfm-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . gfm-mode))

;; ==== MediaWiki Mode ====

(add-to-list 'load-path "~/Documents/mediawiki-el") ;; The bzr clone

(require 'mediawiki)

(autoload 'mediawiki-mode "mediawiki.el" "Major mode for editing MediaWiki files" t)
(setq auto-mode-alist (cons '("\\.mediawiki" . mediawiki-mode) auto-mode-alist))

;; ==== YAML Mode ====

(add-to-list 'load-path "~/Documents/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Commented out stuff "doesn't work"

;; ===== flymake-cursor =====
;; Shows flymake errors in the mode line (since I can't use the mouse)

(require 'flymake-cursor)

;; ===== pos-tip =====
;; Gives better tool-tips to the auto-complete-mode extension.

(require 'pos-tip)

;; ===== python-mode ====

;; (add-to-list 'load-path "~/.emacs.d/python-mode")
;; (setq py-install-directory "~/.emacs.d/python-mode")
;; (require 'python-mode)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; ;; ===== ipython ====
;; ;; Note, this is linked to in ~/.emacs.d/lisp from the ipython git repo, not
;; ;; the dotfiles repo
;;
;; (require 'ipython)

;; The following lazily enables ropemacs (and only loads pymacs with it).
;; Use C-x p l to load ropemacs.  Uncomment the stuff below it to always load them (slow).

;; ==== python.el ====

(add-to-list 'load-path "~/Documents/python.el")
(require 'python)
;; TODO: instead of changing the source to avoid paren indentation, change it
;; here using defadvice.  See http://stackoverflow.com/a/4150438/161801.

;; ==== ropemacs ====
(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
                  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil)
  )
(global-set-key "\C-xpl" 'load-ropemacs)

;; ;; ===== Stuff for the pymacs extension ====
;;
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;;
;; ;; ===== Enable ropemacs =====
;;
;; ;; Uncomment these to prevent ropemacs from changing default keybindings
;; ;; (setq ropemacs-enable-shortcuts nil)
;; ;; (setq ropemacs-local-prefix "C-c C-p")
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")

;; ;; ===== Enable the anything extension ====
;;
;; (require 'anything)
;; (require 'anything-ipython)
;; (when (require 'anything-show-completion nil t)
;;    (use-anything-show-completion 'anything-ipython-complete
;;                                  '(length initial-pattern)))

;; ===== mmm-mode ======
;; Enables sub-modes inside of parts of a buffer

(add-to-list 'load-path "~/Documents/mmm-mode")
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)

;; Make Python docstrings use rst-mode.  See
;; http://stackoverflow.com/q/15493342/161801.  Commented out because there
;; are too many cases where code is rendered as docstrings

;; (mmm-add-classes
;;  '((python-rst
;;     :submode rst-mode
;;     :front "^ *[ru]?\"\"\"$"
;;     :back "^ *\"\"\"$"
;;     :include-front t
;;     :include-back t
;;     :end-not-begin t
;;     :)))
;; (mmm-add-mode-ext-class 'python-mode nil 'python-rst)

;; ==== doctest-mode ====
;; Note, this needs to go below mmm-mode above.

(add-to-list 'load-path "~/Documents/python-mode/python-mode")
(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))
(autoload 'doctest-mode "doctest-mode" "doctest mode" t)
(autoload 'doctest-register-mmm-classes "doctest-mode")
;; (doctest-register-mmm-classes t t)

;; ===== auto-complete-mode ====


(add-to-list 'load-path "~/Documents/auto-complete")
(add-to-list 'load-path "~/Documents/popup-el")
(add-to-list 'load-path "~/Documents/fuzzy-el")
(require 'auto-complete-config)
(require 'popup)
(require 'fuzzy)
(add-to-list 'ac-dictionary-directories "~/Documents/auto-complete/dict")
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(ac-set-trigger-key "TAB")
(ac-flyspell-workaround)
(ac-linum-workaround)
(setq ac-ignore-case nil)
(setq ac-use-menu-map t)
(substitute-key-definition 'ac-next 'next-line ac-menu-map)
(substitute-key-definition 'ac-previous 'previous-line ac-menu-map)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "\C-p") 'ac-previous)
(define-key ac-menu-map (kbd "<backtab>") 'ac-previous)
(substitute-key-definition 'ac-isearch 'isearch-forward ac-menu-map)
(define-key ac-menu-map (kbd "C-c s") 'ac-isearch)
(add-hook 'latex-mode-hook 'auto-complete-mode)
(add-hook 'LaTeX-mode-hook 'auto-complete-mode)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'text-mode-hook 'auto-complete-mode)

;; ==== deferred ====
;; This is needed for EPC

(add-to-list 'load-path "~/Documents/emacs-deferred")
(require 'deferred)

;; ==== ctable ====
;; This is needed for EPC

(add-to-list 'load-path "~/Documents/emacs-ctable")
(require 'ctable)

;; ==== EPC =====
;; This is needed for Jedi

(add-to-list 'load-path "~/Documents/emacs-epc")
(require 'epc)

;; ==== Jedi ====
;; Python completion using Jedi and auto-complete-mode

;; (add-to-list 'load-path "~/Documents/emacs-jedi")
;; (require 'jedi)
;; (autoload 'jedi:setup "jedi" nil t)
;; (global-auto-complete-mode +1)
;; (setq jedi:setup-keys t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
;; (add-to-list 'ac-sources 'ac-source-jedi-direct)
;;
;; ;; !!!!!!!!!!!!!!!!!!!!!!!!!!
;; ;;          WARNING!!!
;; ;; !!!!!!!!!!!!!!!!!!!!!!!!!!
;;
;; ;; The following is needed or else Mac OS X will kernel panic. See
;; ;; https://github.com/tkf/emacs-jedi/issues/37.
;;
;; ;; If you cannot exit emacs because it tells you that jedi:stop-all-servers is
;; ;; not defined, type (setq kill-emacs-hook nil) and type C-x C-e.
;;
;; (defun jedi:stop-all-servers ()
;;     (maphash (lambda (_ mngr) (epc:stop-epc mngr))
;;                         jedi:server-pool--table))
;;
;; (add-hook 'kill-emacs-hook #'jedi:stop-all-servers)

;; ===== Scroll bars ======

;; This is related to auto-complete-mode (same developer)

;; (add-to-list 'load-path "~/Documents/yascroll-el")
;; (require 'yascroll)
;; (global-yascroll-bar-mode 1)

;; ==== AUCTeX ====

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; ==== Predictive ====

;; (add-to-list 'load-path "~/Documents/predictive")
;; (require 'predictive)

;; ===== isearch+ =====

;(require 'isearch+)

;; ===== expand-region =====

(add-to-list 'load-path "~/Documents/expand-region.el")
(require 'expand-region)
(global-set-key (kbd "M-=") 'er/expand-region)

;; ==== multiple-cursors ====

(add-to-list 'load-path "~/Documents/multiple-cursors.el")
(require 'multiple-cursors)
;; f5 and f6 are bound to C-< and C-> in iTerm 2, respectively

(global-set-key [f6] 'mc/mark-next-like-this)
(global-set-key [f5] 'mc/mark-previous-like-this)

;; ==== Highlight indentation =====

(require 'highlight-indentation)

(add-hook 'prog-mode-hook 'highlight-indentation)

;; ==== Undo-tree ====
;; Git repo at http://www.dr-qubit.org/git/undo-tree.git

(add-to-list 'load-path "~/Documents/undo-tree")
(require 'undo-tree)

;; ;; Compress saved undo files
;; (defadvice undo-tree-make-history-save-file-name
;;     (after undo-tree activate)
;;       (setq concat ad-return-value ".gz"))

;; ==== goto-last-change.el
;; http://www.emacswiki.org/emacs/download/goto-last-change.el
;; http://superuser.com/a/184402/39697

(require 'goto-last-change)
(global-set-key (kbd "C-x C-\\") 'goto-last-change)

;; ==== Tabbar mode ====

;; Disabled because I couldn't figure out how to make it do what I want
;; (always show all files by filename)

;; (add-to-list 'load-path "~/Documents/tabbar")
;; (require 'tabbar)

;; =======================================
;; ===== Values set by M-x customize =====

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(auto-save-list-file-prefix "/Users/aaronmeurer/.emacs.d/autosave/")
 '(colon-double-space t)
 '(comment-empty-lines (quote (quote eol)))
 '(cua-enable-cua-keys nil)
 '(cua-enable-modeline-indications t)
 '(cua-enable-region-auto-help nil)
 '(cua-keep-region-after-copy t)
 '(cua-mode nil nil (cua-base))
 '(cua-paste-pop-rotate-temporarily t)
 '(custom-enabled-themes (quote (1am)))
 '(custom-safe-themes (quote ("e48dca04d05f441f1e46c758f01d9ae70d83eca04e6ca8ddd0d82f465ba17a34" "8276ddcf003d6389bdcfb5c2dd8fcdeae1a4ef84f82b68aa4eb3a39ced99229f" "6f7b04d2d3023459b4c8d5cc332a6f0011653da6c44c67ec7f9defa61d9d9702" "3afedf94b282d97db1bba88737d54997e8c3b5aca39af60ad8e0c99280f45468" "05b1b8e7a5b4ed97a9f3d90f80f849da026783d4f68030c14d1f17667382535f" "f71ecd126c6ba71d400e5252f5b83cb463a93da0376deeacf0e19029bde7da8b" "f1cff7b73cccc579c30b880c9f7f07672f4b61e65302f27d052d86dc61b2d15c" "deebfa6364832155c50b5d01a6203a1c0a947901af97ef0bb647bd63fc9986c8" "8dda88444892b832a80a59b0697d8e49c2a6f3487a5f38964d86fbdf3639363b" "94a0edf18f016a4be5b4c4f6207c048ecb6a7c2388578b361d7f405121dd5ddc" "09277acdebe128259f49b20e0cac0f167fc5d21bc0055e011afcbaedf4eee63b" "c1fc09025d628eea1b3fbbc51ca1297c61c689cc6c091a1be187fe1c075a4530" "ad1761ad53fea37fb45993ee3f8d655f52eee8249ea1c91195de702abc88f6b9" "76e393a1b358819802bd21c236d9dff8d2a8fe8295755227c0d85132aeba17c3" "3a930c799b42da0475035d545e34c290c50747e9bd327f007d6a6e2b7e32e8f3" "052e1cc4fe7eb51c63e1096bae6ba1552e2c6ef8b047077a3c1d6d3a5474470a" "2cdbd9a58bad15f1a1d2cbcdb65411d2a8ffe7277caee986f9f7e85ebf9f685d" "984c105aedff7f5d8bee9aae2ad70b7b547957b6a62665bc6de6af0488df6c09" "adf1beba5dd1dcec61c3e080cf0368efbf5e9045b7767e18b27fb9af8682ebc3" "2f54b8f1d15fd066721eb4249ec20cd675b94b34e1fc2721e76b7d5c8d61d2c3" "6f16b05a09588b3e6fcfd30ec258b02a729e581ebb674d2929ff517210694af5" "f81c3986426da442140cc03ff40b104263c6cee2d5b6660394bd3a59f3de7e80" "d97477c58646efd83b364b4614645b42645b12b4c2616ab451b4bcb55200c84d" "58b8232a96b1a6f6b3153ff2038a3c94b37230848c2094e902d555a414c4c293" "dc032b7a76890122b8f67bfccdcdd37cdf4bb505ee8025810535e188ff41b5a9" "b6820d3c35f05f9d4e72b48933141dbcaca5c3dcd04fee7aa3a14bf916d96f21" "94b148d58fe160cb829065dc32c40a254028fc2f6bded2ebcf83c844eb23dc49" "4935aebec6ce9b6b8345e1be34be1c2e321c6f283ddb2bd64f028aa7ce772b8c" "e81394a3cb1468b4e27c6bfd517f4d525694280acf0610ddefea902b9a558f81" "657bb4173efa15dbee1b3406bcacd9e8311983d9870506fad653a07fbe8fb7d2" "b80cd50a6e695872ff71c825dc7d41f457993e9e693ea3931141a02faf7fe646" "079894f67d56b12cfeaf3ae929dc7b486fcfac84127486e581de6e91462596ff" "7cc2ed6fc6b1d9ef1f245745ded639d934e28bb55f70add829ff6bc4bf337da2" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(custom-unlispify-tag-names nil)
 '(delete-selection-mode t)
 '(desktop-locals-to-save (quote (desktop-locals-to-save truncate-lines case-fold-search case-replace fill-column overwrite-mode change-log-default-name line-number-mode column-number-mode size-indication-mode buffer-file-coding-system indent-tabs-mode tab-width indicate-buffer-boundaries indicate-empty-lines show-trailing-whitespace buffer-undo-list)))
 '(desktop-save-mode nil)
 '(doctest-optionflags (quote ("NORMALIZE_WHITESPACE" "ELLIPSIS")))
 '(global-linum-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-silently-savep t)
 '(large-file-warning-threshold nil)
 '(linum-format "%d⎢")
 '(mouse-wheel-scroll-amount (quote (1)))
 '(pcomplete-ignore-case t)
 '(python-fill-docstring-style (quote onetwo))
 '(python-indent-guess-indent-offset nil)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(require-final-newline (quote ask))
 '(save-place t nil (saveplace))
 '(save-place-save-skipped nil)
 '(scroll-step 1)
 '(sentence-end-double-space t)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-trailing-whitespace t)
 '(speedbar-visiting-tag-hook (quote (speedbar-highlight-one-tag-line speedbar-recenter)))
 '(split-height-threshold 80)
 '(split-width-threshold 150)
 '(split-window-keep-point t)
 '(tab-width 4)
 '(tags-case-fold-search t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(which-function-mode t)
 '(window-combination-limit nil)
 '(window-combination-resize t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-jump-face-background ((t (:foreground "gray"))) t)
 '(ace-jump-face-foreground ((t (:foreground "magenta"))) t)
 '(flymake-errline ((t (:foreground "LightPink" :underline "red"))))
 '(flymake-warnline ((t nil)))
 '(ido-first-match ((t (:underline t :weight bold))))
 '(rst-level-1 ((t (:foreground "grey85"))))
 '(rst-level-2 ((t (:foreground "grey78"))))
 '(rst-level-3 ((t (:foreground "grey71"))))
 '(show-paren-match ((t (:background "blue")))))
