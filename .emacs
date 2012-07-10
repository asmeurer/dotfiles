;; Emacs config file

;; ====== TODO ======
;;
;; - Make C-u delete the previous newline when the cursor is at the beginning of
;;   the line.
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
  (define-key latex-mode-map "\C-cw" 'latex-word-count)
  )
(add-hook 'latex-mode-hook 'latex-setup t)

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

(global-set-key "\C-u" 'backward-kill-line)

;; You can still get the original meaning of C-u (universal-argument) with C-c
;; u.  Note, I was going to do C-S-u, but apparently terminals can't
;; distinguish the shift with control

(global-set-key (kbd "C-c u") 'universal-argument)

;; ===== Set C-x C-c to do the right thing in emacsclient
;; TODO

;; ===== Make the *scratch* buffer use text mode by default ====

(setq initial-major-mode 'text-mode)

;; ========== Add a directory to the emacs load-path for extensions =========

(add-to-list 'load-path "~/.emacs.d/lisp/")

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

;; ===== Turn on flyspell-mode ====

;; Use the turn-on-flyspell one to enable it everywhere, and the
;; flyspell-prog-mode to enable it only in comments/strings
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)
(add-hook 'tcl-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'html-mode-hook 'flyspell-prog-mode)
(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))

;; ===== Turn on flymake-mode ====

(add-hook 'c-mode-common-hook 'turn-on-flymake)
(defun turn-on-flymake ()
  "Force flymake-mode on. For us in hooks."
  (interactive)
  (flymake-mode 1))

(add-hook 'c-mode-common-hook 'flymake-keyboard-shortcuts)
(defun flymake-keyboard-shortcuts ()
  "Add keyboard shortcuts for flymake goto next/prev error."
  (interactive)
  (local-set-key "\M-n" 'flymake-goto-next-error)
  (local-set-key "\M-p" 'flymake-goto-prev-error))

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

;; If the LaTeX is too old to have this option, you can use this instead:
;; (defun flymake-get-tex-args (file-name)
;;   (list "chktex" (list "-q" "-v0" file-name)))

(defun flymake-get-tex-args (file-name)
    (list "pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

;; Enable auto-fill-mode for relevant file types

(defun turn-on-auto-fill ()
  "Force auto-fill-mode on. For us in hooks."
  (interactive)
  (auto-fill-mode 1))

(add-hook 'message-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'c-mode-common-hook 'turn-on-auto-fill)
(add-hook 'tcl-mode-hook 'turn-on-auto-fill)
(add-hook 'python-mode-hook 'turn-on-auto-fill)
(add-hook 'lisp-mode-hook 'turn-on-auto-fill)
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; ===== Enable mouse support (?) ====

(require 'xt-mouse)
(xterm-mouse-mode)
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))

;; ===== Highlight tabs ====
(standard-display-ascii ?\t ">>>|")
(add-hook 'text-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\t" 0 'trailing-whitespace prepend)))))

;; ===== Extensions stuff =======
;; ==============================

(add-to-list 'load-path "~/Documents/smex") ;; The git clone
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x M-x") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; ==== Markdown mode =====

(add-to-list 'load-path "~/Documents/markdown-mode") ;; The git clone

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

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
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)


;; TODO: Do something like at
;; http://www.enigmacurry.com/2009/01/21/autocompleteel-python-code-completion-in-emacs/
;; to make this also be smart about Python (e.g., complete after . in a module
;; name).

;; ===== Scroll bars ======

;; This is related to auto-complete-mode (same developer)

(add-to-list 'load-path "~/Documents/yascroll-el")
(require 'yascroll)
;;(global-yascroll-bar-mode 1)

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

(require 'isearch+)

;; ==== Highlight indentation =====

(require 'highlight-indentation)

(add-hook 'python-mode-hook 'highlight-indentation)
(add-hook 'c-mode-common-hook 'highlight-indentation)

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
 '(cua-keep-region-after-copy t)
 '(cua-mode t nil (cua-base))
 '(cua-paste-pop-rotate-temporarily t)
 '(custom-enabled-themes (quote (1am)))
 '(custom-safe-themes (quote ("09277acdebe128259f49b20e0cac0f167fc5d21bc0055e011afcbaedf4eee63b" "c1fc09025d628eea1b3fbbc51ca1297c61c689cc6c091a1be187fe1c075a4530" "ad1761ad53fea37fb45993ee3f8d655f52eee8249ea1c91195de702abc88f6b9" "76e393a1b358819802bd21c236d9dff8d2a8fe8295755227c0d85132aeba17c3" "3a930c799b42da0475035d545e34c290c50747e9bd327f007d6a6e2b7e32e8f3" "052e1cc4fe7eb51c63e1096bae6ba1552e2c6ef8b047077a3c1d6d3a5474470a" "2cdbd9a58bad15f1a1d2cbcdb65411d2a8ffe7277caee986f9f7e85ebf9f685d" "984c105aedff7f5d8bee9aae2ad70b7b547957b6a62665bc6de6af0488df6c09" "adf1beba5dd1dcec61c3e080cf0368efbf5e9045b7767e18b27fb9af8682ebc3" "2f54b8f1d15fd066721eb4249ec20cd675b94b34e1fc2721e76b7d5c8d61d2c3" "6f16b05a09588b3e6fcfd30ec258b02a729e581ebb674d2929ff517210694af5" "f81c3986426da442140cc03ff40b104263c6cee2d5b6660394bd3a59f3de7e80" "d97477c58646efd83b364b4614645b42645b12b4c2616ab451b4bcb55200c84d" "58b8232a96b1a6f6b3153ff2038a3c94b37230848c2094e902d555a414c4c293" "dc032b7a76890122b8f67bfccdcdd37cdf4bb505ee8025810535e188ff41b5a9" "b6820d3c35f05f9d4e72b48933141dbcaca5c3dcd04fee7aa3a14bf916d96f21" "94b148d58fe160cb829065dc32c40a254028fc2f6bded2ebcf83c844eb23dc49" "4935aebec6ce9b6b8345e1be34be1c2e321c6f283ddb2bd64f028aa7ce772b8c" "e81394a3cb1468b4e27c6bfd517f4d525694280acf0610ddefea902b9a558f81" "657bb4173efa15dbee1b3406bcacd9e8311983d9870506fad653a07fbe8fb7d2" "b80cd50a6e695872ff71c825dc7d41f457993e9e693ea3931141a02faf7fe646" "079894f67d56b12cfeaf3ae929dc7b486fcfac84127486e581de6e91462596ff" "7cc2ed6fc6b1d9ef1f245745ded639d934e28bb55f70add829ff6bc4bf337da2" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(custom-unlispify-tag-names nil)
 '(global-linum-mode t)
 '(global-subword-mode t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-silently-savep t)
 '(large-file-warning-threshold nil)
 '(linum-format "%d ")
 '(mouse-wheel-scroll-amount (quote (1)))
 '(pcomplete-ignore-case t)
 '(python-guess-indent nil)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(require-final-newline (quote ask))
 '(save-place t nil (saveplace))
 '(save-place-save-skipped nil)
 '(save-place-skip-check-regexp "\\`/\\(?:cdrom\\|floppy\\|mnt\\|\\(?:[^@/:]*@\\)?[^@/:]*[^@/:.]\\|.*COMMIT_EDITMSG:\\)")
 '(scroll-step 1)
 '(sentence-end-double-space t)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-trailing-whitespace t)
 '(speedbar-visiting-tag-hook (quote (speedbar-highlight-one-tag-line speedbar-recenter)))
 '(tab-width 4)
 '(tags-case-fold-search t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:foreground "LightPink" :underline "red"))) t)
 '(flymake-warnline ((t nil)) t)
 '(ido-first-match ((t (:underline t :weight bold)))))
