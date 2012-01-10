;; Emacs config file

;; Thanks to http://homepages.inf.ed.ac.uk/s0243221/emacs/ for many of these

;; ========== Add a directory to the emacs load-path for extensions =========

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; ========== Line by line scrolling ==========

;; This makes the buffer scroll by only a single line when the up or
;; down cursor keys push the cursor (tool-bar-mode) outside the
;; buffer. The standard emacs behaviour is to reposition the cursor in
;; the center of the screen, but this can make the scrolling confusing

(setq scroll-step 1)

;; ========== Enable Line and Column Numbering ==========

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

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

;; TODO: Turn this on only for relevant file types

(setq flyspell-mode 1)

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


;; ===== Extensions stuff =======
;; ==============================

;; ===== python-mode ====

(add-to-list 'load-path "~/.emacs.d/python-mode")
(setq py-install-directory "~/.emacs.d/python-mode")
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; ===== ipython ====
;; Note, this is linked to in ~/.emacs.d/lisp from the ipython git repo, not
;; the dotfiles repo

(require 'ipython)

;; ===== Stuff for the pymacs extension ====

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;; ===== Values set by M-x customize =====

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(abbrev-mode t)
 '(colon-double-space t)
 '(comment-empty-lines (quote (quote eol)))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(sentence-end-double-space nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
