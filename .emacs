;; Emacs config file

;; Thanks to http://homepages.inf.ed.ac.uk/s0243221/emacs/ for many of these

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
    "Kill chars backward until encountering the end of a line."
      (interactive "p")
        (kill-line 0))

;; I don't use C-u for it's normal use, but I do use this macro.
(global-set-key "\C-u" 'backward-kill-line)

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
(defun turn-on-flyspell ()
   "Force flyspell-mode on using a positive arg.  For use in hooks."
   (interactive)
   (flyspell-mode 1))

;; ===== Automatically indent with RET =====

(define-key global-map (kbd "RET") 'newline-and-indent)

;; ===== Clear trailing whitespace on save ====

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ===== Use four spaces instead of tabs ====

(setq c-basic-indent 2)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; ===== Trailing whitespace ======

(setq-default highlight-trailing-whitespace)

;; ===== Highlight Marked Text =====

(setq-default transient-mark-mode t)

;; ===== Abbreviations =====

;; For some reason this doesn't work

(define-abbrev global-abbrev-table "Ondrej" "Ondřej")
(define-abbrev global-abbrev-table "Certik" "Čertík")

;; ===== Flymake for tex-mode ====

;; flymake-mode for tex uses texify by default, which only works in Windows (miktex)

;; If the LaTeX is too old to have this option, you can use this instead:
;; (defun flymake-get-tex-args (file-name)
;;   (list "chktex" (list "-q" "-v0" file-name)))

(defun flymake-get-tex-args (file-name)
    (list "pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

;; ===== Enable mouse support (?) ====

(require 'xt-mouse)
(xterm-mouse-mode)

;; ===== Extensions stuff =======
;; ==============================

;; ==== Markdown mode =====

(add-to-list 'load-path "~/Documents/markdown-mode") ;; The git clone

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Commented out stuff "doesn't work"

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

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/ac-dict")
(ac-config-default)
;;(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(ac-set-trigger-key "TAB")
(ac-flyspell-workaround)

;; TODO: Do something like at
;; http://www.enigmacurry.com/2009/01/21/autocompleteel-python-code-completion-in-emacs/
;; to make this also be smart about Python (e.g., complete after . in a module
;; name).

;; ==== AUCTeX ====

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; ==== Predictive ====

(add-to-list 'load-path "~/Documents/predictive")
(require 'predictive)

;; ===== isearch+ =====

(require 'isearch+)

;; ==== Highlight indentation =====

(require 'highlight-indentation)

(add-hook 'python-mode-hook 'highlight-indentation)

;; =======================================
;; ===== Values set by M-x customize =====

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(colon-double-space t)
 '(comment-empty-lines (quote (quote eol)))
 '(custom-unlispify-tag-names nil)
 '(global-linum-mode t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-silently-savep t)
 '(large-file-warning-threshold nil)
 '(linum-format "%d ")
 '(mouse-wheel-scroll-amount (quote (1)))
 '(pcomplete-ignore-case t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case nil)
 '(scroll-step 1)
 '(sentence-end-double-space nil)
 '(show-trailing-whitespace t)
 '(tags-case-fold-search t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indent-face ((t (:background "color-233")))))
