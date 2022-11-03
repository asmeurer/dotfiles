;; Emacs config file

;; ====== TODO ======
;;
;; - Highlight 'single quoted' text and "double quoted" text differently in
;;   Python.
;; - Export all face changes to the theme.
;; - Fix TeXcount.
;; - Fix bug with isearch when the search is not found and you type delete.
;; - Fix inconsistency between C-x # and C-x C-c in emacsclient.
;; - Make C-SPC unset the mark when selection is active.
;; - Find a way to let the cursor move off screen.
;; - Make SyntaxErrors in Python show the relevant character
;; - Figure out escape codes entered in files ([<8;32;21m)

;; Thanks to http://homepages.inf.ed.ac.uk/s0243221/emacs/ for many of these

;; ===== auto-compile ====
;; Should automatically recompile .el files
;; This should stay at the top of this file.

;; ;; -*- no-byte-compile: t -*-
;; (add-to-list 'load-path "~/Documents/auto-compile")
;; (require 'auto-compile)
;; (auto-compile-on-load-mode 1)
;; (auto-compile-on-save-mode 1)

;; Don't show the splash screen on startup.  Aside from me not needing it
;; anymore, this fixes a bug with emacsclient.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-splash-screen t)

;; ========== Add a directory to the emacs load-path for extensions =========

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; ==== Set the font as Menlo =====
;; https://github.com/hbin/top-programming-fonts

(set-frame-font "Menlo:pixelsize=18")

(add-to-list 'default-frame-alist
             (cons 'font "Menlo:pixelsize=18")
             (cons 'width 123)
             (cons 'height 72))

;; ==== use-package ====

;; ;; Bootstrap use-package. From
;; ;; https://swsnr.de/posts/my-emacs-configuration-with-use-package
;;
;; (require 'package)
;; (setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;;
;; (package-initialize)
;;
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; Alternate use-package init, from git (https://jwiegley.github.io/use-package/installation/)

(add-to-list 'load-path "~/Documents/use-package")
(require 'use-package)

;; Keep the package repository certs up-to-date (otherwise it might fail to
;; connect if they expire)
;; (use-package gnu-elpa-keyring-update
;;   :config (gnu-elpa-keyring-update))

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/Documents/use-package/"))

;; Make packages always auto-install
(setq use-package-always-ensure t)
;; It doesn't seem to work for some reason, so we use :ensure t below.

;; Install various packages

;; ==== command-log-mode ====

;; Provides a nicer way to show keyboard output for demos
(use-package command-log-mode)
(setq clm/log-command-exceptions* '(nil))

;; ==== flycheck ====

(use-package flycheck
  :bind
  (("M-n" . flycheck-next-error)
   ("M-p" . flycheck-previous-error))
  :custom
  (flycheck-disabled-checkers '(python-flake8 python-pylint)))

;; ===== flycheck-pyflakes ======

(use-package flycheck-pyflakes)

;; ==== Undo-tree ====
;; Git repo at http://www.dr-qubit.org/git/undo-tree.git

;; Installed by use-package
;; (add-to-list 'load-path "~/Documents/undo-tree")
;; (require 'undo-tree)

;; C-S-/ has to pass through this escape code with iTerm2.

(use-package undo-tree
  :bind
  ("C-[ [ a b" . undo-tree-redo)
  :custom
  ((global-undo-tree-mode t)
   (undo-tree-auto-save-history t)
   (undo-tree-history-directory-alist (quote ((".*" . "~/.emacs.d/undo-tree/"))))))

;; ;; Compress saved undo files
;; (defadvice undo-tree-make-history-save-file-name
;;     (after undo-tree activate)
;;       (setq concat ad-return-value ".gz"))

;; ==== multiple-cursors ====

;; Installed by use-package
;; (add-to-list 'load-path "~/Documents/multiple-cursors.el")
;; (require 'multiple-cursors)
;; f5 and f6 are bound to C-< and C-> in iTerm 2, respectively

(use-package multiple-cursors
  :bind
  (([f6] . mc/mark-next-like-this)
   ([f5] . mc/mark-previous-like-this)))

;; ==== aggressive-indent-mode ====
;;
;; Installed by use-package
;;
;; (add-to-list 'load-path "~/Documents/aggressive-indent-mode")
;; (require 'aggressive-indent)
(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  :custom
  (aggressive-indent-excluded-modes '(markdown-mode python-mode makefile-mode diff-mode)))

;; ==== MediaWiki Mode ====

;; (add-to-list 'load-path "~/Documents/mediawiki-el") ;; The bzr clone
;;
;; ;; (require 'mediawiki)
;;

;; Commented out because it doesn't seem to be working. But I don't need it
;; for now.

(use-package mediawiki
  :mode ("\\.mediawiki\\'" . mediawiki-mode))

;; === Anzu ====

;; Show the total number of search results

;; (add-to-list 'load-path "~/Documents/emacs-anzu")
;; (require 'anzu)
(use-package anzu)
;; :config
;; (global-anzu-mode +1))

;; ==== bug-hunter ====
(use-package bug-hunter)

;; ==== cython-mode ====
(use-package cython-mode
  :mode
  ("\\.pyx\\'"
   "\\.pxd\\'"
   "\\.pxi\\'"))

;; ==== ido-sort-mtime ====

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1))

(use-package ido-sort-mtime
  :config
  (ido-sort-mtime-mode 1))

;; ==== auctex ====
(use-package tex
  :ensure auctex
  :defer t)

;; ==== magit ====

;; (use-package magit)

;; ==== git-gutter ====

(use-package git-gutter)

;; ;; ==== unmodified-buffer
;;
;; (use-package unmodified-buffer
;;   :straight (:host github :repo "arthurcgusmao/unmodified-buffer")
;;   :hook (after-init . unmodified-buffer-mode))

;; ;; ==== filetree ====
;;
;; (use-package filetree)

;; ==== cask-mode =====

;; We don't use cask any more but cask-mode can still be useful for editing
;; the Cask files

(use-package cask-mode)

;; ===== Smart comment =====

;; https://emacs.stackexchange.com/questions/16792/easiest-way-to-check-if-current-line-is-empty-ignoring-whitespace
(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun smart-comment-end (arg)
  "Same as smart-comment-line but comments if the line is empty"
  (interactive "*P")
  (if (current-line-empty-p) (comment-dwim arg) (smart-comment-line arg)))

(use-package smart-comment
  :custom
  (smart-comment-end-action 'smart-comment-end)
  :bind
  ("M-;" . smart-comment))

;; ==== sml-modeline ====
;; Puts a progress bar on the mode line

(use-package sml-modeline
  :custom
  (sml-modeline-len 17)
  (sml-modeline-mode t)
  :custom-face
  (sml-modeline-end-face ((t (:background "black" :foreground "white"))))
  (sml-modeline-vis-face ((t (:inherit yascroll:thumb-text-area)))))

;; ==== dockerfile-mode ====

(use-package dockerfile-mode)

;; ===== Turn on flyspell-mode ====

(use-package flyspell-lazy
  :config
  (flyspell-lazy-mode 1)
  :custom
  (flyspell-lazy-changes-threshold 10)
  (flyspell-lazy-idle-seconds 0.1)
  (flyspell-lazy-less-feedback t)
  (flyspell-lazy-mode t)
  (flyspell-lazy-size-threshold 5)
  (flyspell-lazy-use-flyspell-word nil)
  (flyspell-lazy-window-idle-seconds 1))

;; ==== cmake-font-lock ====
;; Enables syntax highlighting for cmake files
(use-package cmake-font-lock)

;; ==== latex-extra ====

;; https://github.com/Malabarba/latex-extra

;; Enables content folding (hit TAB on section headers) and some other
;; features as well.
(use-package latex-extra
  :hook (LaTeX-mode . latex-extra-mode))

;; ==== avy ====

;; Replacement for ace-jump-mode. Type C-x SPC then some characters to
;; navigate around

(use-package avy
  :bind
  ("C-x SPC" . avy-goto-char)
  ("C-'" . avy-goto-char-timer))

;; ==== ace-window ====
;; Like avy but for switching windows

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  )

;; ==== Markdown mode =====

(add-to-list 'load-path "~/Documents/markdown-mode") ;; The git clone


(defun markdown-fill-paragraph-plain-text (&optional arg)
  "Run `fill-paragraph' as if we were in `text-mode'.

ARG is the same as with `fill-paragraph'."
  (interactive "P")
  (unwind-protect
      (progn (text-mode)
             (fill-paragraph arg))
    (markdown-mode)))

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown
files" t)
(autoload 'gfm-mode "markdown-mode.el" "Major mode for editing GitHub flavored
Markdown" t)
(use-package markdown-mode
  :mode
  ("\\.md" . gfm-mode)
  ("\\.markdown" . gfm-mode)
  ("PULLREQ_EDITMSG" . gfm-mode)
  ("COMMIT_EDITMSG" . gfm-mode)
  ("TAG_EDITMSG" . gfm-mode)
  :bind
  (:map markdown-mode-map
        ("M-q" . markdown-fill-paragraph-plain-text)
        ("M-p" . flycheck-previous-error)
        ("M-n" . flycheck-next-error)))




;; Enable spell checking in Markdown code blocks
;; This is required to support spell checking inside of MyST directive blocks.

(defun markdown-flyspell-check-word-myst-p ()
  "Return t if `flyspell' should check word just before point.
Used for `flyspell-generic-check-word-predicate'. Based on
  `markdown-flyspell-check-word-p', but allows spelling inside of code blocks"
  (save-excursion
    (goto-char (1- (point)))
    (not (or
          ;; (markdown-code-block-at-point-p)
          (markdown-inline-code-at-point-p)
          ;; (markdown-in-comment-p)
          (let ((faces (get-text-property (point) 'face)))
            (if (listp faces)
                (or (memq 'markdown-reference-face faces)
                    (memq 'markdown-markup-face faces)
                    (memq 'markdown-plain-url-face faces)
                    (memq 'markdown-inline-code-face faces)
                    (memq 'markdown-url-face faces))
              (memq faces '(markdown-reference-face
                            markdown-markup-face
                            markdown-plain-url-face
                            markdown-inline-code-face
                            markdown-url-face))))))))

(advice-add 'markdown-flyspell-check-word-p :override #'markdown-flyspell-check-word-myst-p)

(setq flyspell-generic-check-word-predicate
      #'markdown-flyspell-check-word-myst-p)

;; ==== YAML Mode ====

(use-package yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; ===== Coffeescript ====

(use-package coffee-mode)

;; ===== sass ======

(use-package sass-mode
  :mode "\\.sass\\'")

;; ===== auto-complete-mode ====

(use-package auto-complete
  :config
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (ac-flyspell-workaround)
  (ac-linum-workaround)
  (setq ac-use-fuzzy nil)
  (setq ac-ignore-case nil)
  (setq ac-use-menu-map t)
  (substitute-key-definition 'ac-next 'next-line ac-menu-map)
  (substitute-key-definition 'ac-previous 'previous-line ac-menu-map)
  (substitute-key-definition 'ac-isearch 'isearch-forward ac-menu-map)
  (global-auto-complete-mode +1)
  :bind
  (:map
   ac-menu-map
   ;; ("C-n" . ac-next)
   ;; ("\C-p" . ac-previous)
   ("<backtab>" . ac-previous)
   ("C-c s" . ac-isearch)
   :map
   ac-completing-map
   ("\r" . nil)
   ("<up>" . nil)
   ("<down>" . nil))
  ;; :map
  ;; ac-mode-map ("M-TAB" . auto-complete)
  :hook
  (latex-mode . auto-complete-mode)
  (LaTeX-mode . auto-complete-mode)
  (prog-mode . auto-complete-mode)
  (text-mode . auto-complete-mode))


;; ==== Jedi ====
;; Python completion using Jedi and auto-complete-mode

(use-package jedi
  :config
  (setq jedi:use-shortcuts t)
  (setq jedi:complete-on-dot t)
  (add-to-list 'ac-sources 'ac-source-jedi-direct)
  ;; Doesn't work yet. See https://github.com/tkf/emacs-jedi/issues/53.
  (setq jedi:install-imenu nil)
  (setq jedi:imenu-create-index-function 'jedi:create-flat-imenu-index)
  (setq jedi:server-args
        '("--log-level" "DEBUG"
          "--log-traceback"))
  ;; Disable Jedi function tooltips. Can use
  ;;
  ;; (setq jedi:tooltip-method nil)
  ;;
  ;; instead to make it show up in the minibuffer (the default is a popup). We
  ;; disable it because the tooltips are annoying, and the minibuffer stuff
  ;; overwrites more useful things like flycheck.
  (setq jedi:get-in-function-call-delay 100000)
  :custom
  (jedi:server-command
   `("~/Documents/emacs-jedi/env/bin/python" ,(expand-file-name "~/Documents/emacs-jedi/jediepcserver.py")))
  ;; C-M-i is currently bound to flyspell-auto-correct-word
  ;; :bind
  ;; (:map
  ;;  python-mode-map
  ;;  ;; M-TAB
  ;;  ("C-M-i" . jedi:complete))
  :hook
  (python-mode . jedi:setup))

;; ==== popwin ====
;; Make annoying popup windows go away better

(use-package popwin
  :config
  (popwin-mode 1))

;; ==== Visual regexp ====

(use-package visual-regexp
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/select-query-replace)
   ;; if you use multiple-cursors, this is for you:
   ("C-c m" . vr/mc-mark)))

(use-package visual-regexp-steroids
  :bind
  ;; use visual-regexp-steroids's isearch instead of the built-in regexp
  ;; isearch

  ;; Commented out because it makes C-x C-s not work inside of isearch. See
  ;; https://github.com/benma/visual-regexp.el/issues/56. You can still use
  ;; C-M-s and C-M-r to get the regexp isearch.

  ;; (("C-r"  . vr/isearch-backward)
  ;;  ("C-s" . vr/isearch-forward))

  (("C-M-r"  . vr/isearch-backward)
   ("C-M-s" . vr/isearch-forward))

  :config
  ;; Make vr--isearch always case insensitive
  (defadvice vr--isearch (around add-case-insensitive (forward string &optional bound noerror count) activate)
    (setq string (concat "(?i)" string))
    ad-do-it))

;; ===== expand-region =====

(use-package expand-region
  :bind
  ("M-=" . er/expand-region))

;; ;; ==== didyoumean.el ====
;;
;; (use-package didyoumean)

;; ==== auto-package-update ====

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  ;; (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; ====== visible-mark =====
(use-package visible-mark)

;; ==== highlight-symbol ====

(use-package highlight-symbol
  :bind
  ("M-N" . highlight-symbol-next)
  ("M-P" . highlight-symbol-prev)
  :custom
  (highlight-symbol-idle-delay 0)
  :hook
  (prog-mode . highlight-symbol-mode))

;; ===== highlight-numbers ======

(use-package highlight-numbers
  :hook
  (prog-mode . highlight-numbers-mode))

;; ==== dumb jump ====

(use-package ivy)

(use-package dumb-jump
  :hook
  (xref-backend-functions . 'dumb-jump-xref-activate)
  ;; :bind (
  ;;        ;; ("M-g o" . dumb-jump-go-other-window)
  ;;        ("M-." . dumb-jump-go)
  ;;        ("M-," . dumb-jump-back)
  ;;        ;; ("M-g i" . dumb-jump-go-prompt)
  ;;        ;; ("M-g x" . dumb-jump-go-prefer-external)
  ;;        ;; ("M-g z" . dumb-jump-go-prefer-external-other-window)
  ;;        )
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  )

;; ==== Auto dim =====

;; (use-package auto-dim-other-buffers
;;   :hook
;;   (after-init . (lambda ()
;;                   (when (fboundp 'auto-dim-other-buffers-mode)
;;                     (auto-dim-other-buffers-mode t)))))

;; ;; Enable this when https://github.com/gonewest818/dimmer.el/pull/16 is merged.
;; (use-package dimmer
;;   :defer 1
;;   :config
;;   ;; (setq dimmer-fraction 0.50)
;;   (dimmer-mode t))

;; Highlight non-ascii chars

;; https://www.emacswiki.org/emacs/ShowWhiteSpace#HighlightChars

;; (require 'highlight-chars)
;;
;; (defface nonascii-face
;;   '((t (:background "color-160")))
;;   "Face for highlighting nonascii characters"
;;   :group 'text-mode)
;;
;; (defvar highlight-nonascii-p nil
;;   "Non-nil means font-lock mode highlights nonascii characters.")
;;
;; (defun toggle-highlight-nonascii (&optional msgp)
;;   "Toggle highlighting of non-ASCII characters.
;; Uses face `nonascii-face'."
;;   (interactive "p")
;;   (setq highlight-nonascii-p  (not highlight-nonascii-p))
;;   (if highlight-nonascii-p
;;       (add-hook 'font-lock-mode-hook 'highlight-nonascii)
;;     (remove-hook 'font-lock-mode-hook 'highlight-nonascii)
;;     (dont-highlight-nonascii))
;;   (font-lock-mode) (font-lock-mode)
;;   (when msgp (message "non-ASCII highlighting is now %s"
;;                       (if highlight-nonascii-p "ON" "OFF"))))
;;
;; (defun highlight-nonascii ()
;;   "Highlight whitespace characters."
;;   (when (boundp 'nobreak-char-display) (setq nobreak-char-display  nil))
;;   (font-lock-add-keywords nil
;;                           `(("[:nonascii:]" (0 'nonascii-face
;;                                                ,hc-font-lock-override))) 'APPEND))
;;
;; (defun dont-highlight-nonascii ()
;;   "Do not highlight nonascii characters."
;;   (when (fboundp 'font-lock-remove-keywords)
;;     (font-lock-remove-keywords nil `(("[:nonascii:]" (0 'nonascii-face ,hc-font-lock-override))))
;;     ))
;;
;; (add-hook 'change-major-mode-hook
;;           (lambda ()
;;             (add-hook 'font-lock-mode-hook
;;                       'highlight-nonascii)))
;;
;; (add-hook 'after-change-major-mode-hook
;;           (lambda ()
;;             (when (eq major-mode 'THE-MODE)
;;               (remove-hook 'font-lock-mode-hook
;;                            'highlight-nonascii)
;;               (dont-highlight-nonascii)))
;;           'APPEND)

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

;; Commands to interact with the clipboard

(defun osx-copy ()
  (interactive)
  (let ((deactivate-mark t))
    (call-process-region (point) (mark) "pbcopy")))

(defun osx-paste ()
  (interactive)
  (if (region-active-p) (delete-region (region-beginning) (region-end)) nil)
  (call-process "pbpaste" nil t nil))

(defun linux-copy (beg end)
  (interactive "r")
  (call-process-region beg end  "xclip" nil nil nil "-selection" "c"))

(defun linux-paste ()
  (interactive)
  (if (region-active-p) (delete-region (region-beginning) (region-end)) nil)
  (call-process "xsel" nil t nil "-b"))

(cond
 ((string-equal system-type "darwin") ; Mac OS X
  (define-key global-map (kbd "C-x C-w") 'osx-copy)
  (define-key global-map (kbd "C-x C-y") 'osx-paste))
 ((string-equal system-type "gnu/linux") ; linux
  (define-key global-map (kbd "C-x C-w") 'linux-copy)
  (define-key global-map (kbd "C-x C-y") 'linux-paste)))


;; Better zapping

;; Load zap-up-to-char from the misc package that comes with emacs
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

;; Make M-z zap-up-to-char (doesn't include char)
(global-set-key "\M-z" 'zap-up-to-char)
;; Make M-Z zap in reverse
(defun reverse-zap-up-to-char (char)
  "Like zap-up-to-char with argument -1"
  (interactive "cZap back to char: ")
  (zap-up-to-char -1 char))

(global-set-key "\M-Z" 'reverse-zap-up-to-char)

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

;; Disabled abbrevs in rst-mode that add things to words like "contents"
;; whenever they are typed.

(add-hook 'rst-mode-hook (lambda () (clear-abbrev-table rst-mode-abbrev-table)))

;; Prevent emacs from asking about saving abbrevs every time it closes. Any
;; abbrevs that should be saved persistantly should be added to this file.
(setq save-abbrevs nil)

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

;; Make M-g TAB (move-to-column) fill spaces past the end of the line
;; From http://emacsredux.com/blog/2013/07/09/go-to-column/

(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))

(global-set-key (kbd "M-g TAB") 'go-to-column)

;; Make C-x f work like C-x C-f, since I always accidentally type the former
;; and never need set-fill-column.
(global-set-key (kbd "C-x f") 'ido-find-file)

;; Make C-c C-x work the same as C-x C-c, since I always mistype them.
(global-set-key (kbd "C-c C-x") 'save-buffers-kill-terminal)

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

;; Better yank behavior. Make M-y also add the item to the front of the kill
;; ring. From https://stackoverflow.com/a/31867563/161801

;; ----------------------------------------------------------------
;; helper functions

(defun list-insert-before (l n x)
  (if (<= n 0) (cons x l)
    (cons (car l) (list-insert-before (cdr l) (- n 1) x))))

(defun list-prepend-nth (l n)
  (if (<= n 0) l
    (let* ((lx (list-prepend-nth (cdr l) (- n 1))))
      (cons (car lx) (cons (car l) (cdr lx))))))

(defun list-insert-car-at (l n)
  (list-insert-before (cdr l) n (car l)))


;; ----------------------------------------------------------------
;; overriding current-kill

(defvar kill-ring-yank-index 0
  "Index into kill-ring of last yank-pop. The item yank-popped
  will be at the head of the kill ring, but if the next command
  is also yank-pop, it will be returned here first before this
  variable is incremented.")

;; Disabled because it breaks Cmd-v

;; (defun current-kill (n)
;;   "Replaces standard 'current-kill' function. This version tries
;; to fix the increasing yank-pop problem.
;;
;; TODO:
;; - respect second argument of original function
;; - deal with 'interprogram-{cut,paste}-function'
;; "
;;   (if (eq 0 n) ;; looks like we're doing a yank; reset
;;       ;; kill-ring-yank-index to 0 to indicate that the
;;       ;; current head of the list is useful to the user
;;       (progn (setq kill-ring-yank-index 0)
;;              (car kill-ring))
;;
;;     ;; otherwise put the head of kill-ring back where we had
;;     ;; previously found it, and fetch the next element
;;     (setq kill-ring
;;           (list-insert-car-at kill-ring kill-ring-yank-index))
;;     (setq kill-ring-yank-index (+ kill-ring-yank-index n))
;;     (when (>= kill-ring-yank-index (- (length kill-ring) 1))
;;       (setq kill-ring-yank-index (- (length kill-ring) 1))
;;       (message "Reached end of kill-ring"))
;;     (when (< kill-ring-yank-index 0)
;;       (setq kill-ring-yank-index 0)
;;       (message "Reached beginning of kill-ring"))
;;     (setq kill-ring (list-prepend-nth kill-ring kill-ring-yank-index))
;;     (car kill-ring)))

;; ----------------------------------------------------------------
;; new key binding

;; Here's an auxiliary function and key binding that makes it easy to
;; go back and forth in the kill-ring while we're yank-popping

;; (defun yank-pop-back () "" (interactive "*")
;;        (yank-pop -1))
;;
;; (global-set-key "\C-\M-y" 'yank-pop-back)

;; You can still get the original meaning of C-u (universal-argument) with C-c
;; u.  Note, I was going to do C-S-u, but apparently terminals can't
;; distinguish the shift with control.  But see below for a workaround.

(global-set-key (kbd "C-c u") 'universal-argument)

;; I have iTerm 2 set to make Shift-Control-U send f8.
(global-set-key [f8] 'universal-argument)

(global-set-key (kbd "C-S-u") 'universal-argument)

;; Make Home and End go to the beginnng/end of the document, not line
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

;; Make insert not do anything

(global-unset-key [insertchar])

;; ====== Set Terminal keyboard shortcuts =====

;; To add a keyboard shortcut that isn't supported by the terminal, like
;; C-S-*, add a shortcut for the respective escape sequence (%d is the ASCII
;; code for the uppercase letter, e.g., C-S-u is \E[27;6;85~

;; For konsole, the shortcuts are in .local/share/konsole/default.keytab, like
;;
;;     key U+Shift+Ctrl : "\E[27;6;85~"

;; From https://emacs.stackexchange.com/questions/1020/problems-with-keybindings-when-using-terminal
;; xterm with the resource ?.VT100.modifyOtherKeys: 1
;; GNU Emacs >=24.4 sets xterm in this mode and define
;; some of the escape sequences but not all of them.
(defun character-apply-modifiers (c &rest modifiers)
  "Apply modifiers to the character C.
MODIFIERS must be a list of symbols amongst (meta control shift).
Return an event vector."
  (if (memq 'control modifiers) (setq c (if (or (and (<= ?@ c) (<= c ?_))
                                                (and (<= ?a c) (<= c ?z)))
                                            (logand c ?\x1f)
                                          (logior (lsh 1 26) c))))
  (if (memq 'meta modifiers) (setq c (logior (lsh 1 27) c)))
  (if (memq 'shift modifiers) (setq c (logior (lsh 1 25) c)))
  (vector c))
(defun my-eval-after-load-xterm ()
  (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
    (let ((c 32))
      (while (<= c 126)
        (mapc (lambda (x)
                (define-key xterm-function-map (format (car x) c)
                  (apply 'character-apply-modifiers c (cdr x))))
              '(;; with ?.VT100.formatOtherKeys: 0
                ("\e\[27;3;%d~" meta)
                ("\e\[27;5;%d~" control)
                ("\e\[27;6;%d~" control shift)
                ("\e\[27;7;%d~" control meta)
                ("\e\[27;8;%d~" control meta shift)
                ;; with ?.VT100.formatOtherKeys: 1
                ("\e\[%d;3u" meta)
                ("\e\[%d;5u" control)
                ("\e\[%d;6u" control shift)
                ("\e\[%d;7u" control meta)
                ("\e\[%d;8u" control meta shift)))
        (setq c (1+ c))))))
(eval-after-load "xterm" '(my-eval-after-load-xterm))

;; ===== iTerm2 keys ====

;; Taken from the iterm mailing list. You need to set these up in the iTerm
;; settings to enable them. For example, C-. is "5.

(defun chopps-add-local-keys (&optional frame)
  (let ((keymap function-key-map))    ; was local-function-key-map (message "adding keys")
    (define-key keymap "\e[1;2A" [S-up])
    (define-key keymap "\e[1;2B" [S-down])
    (define-key keymap "\e[1;2C" [S-right])
    (define-key keymap "\e[1;2D" [S-left])
    (define-key keymap "\e[1;3A" [M-up])
    (define-key keymap "\e[1;3B" [M-down])
    (define-key keymap "\e[1;3C" [M-right])
    (define-key keymap "\e[1;3D" [M-left])
    (define-key keymap "\e[1;9A" [M-up])
    (define-key keymap "\e[1;9B" [M-down])
    (define-key keymap "\e[1;9C" [M-right])
    (define-key keymap "\e[1;9D" [M-left])
    (define-key keymap "\e[1;5A" [C-up])
    (define-key keymap "\e[1;5B" [C-down])
    (define-key keymap "\e[1;5C" [C-right])
    (define-key keymap "\e[1;5D" [C-left])
    (define-key keymap "\e[1;6A" [C-S-up])
    (define-key keymap "\e[1;6B" [C-S-down])
    (define-key keymap "\e[1;6C" [C-S-right])
    (define-key keymap "\e[1;6D" [C-S-left])
    (define-key keymap "\e[1;4A" [M-S-up])
    (define-key keymap "\e[1;4B" [M-S-down])
    (define-key keymap "\e[1;4C" [M-S-right])
    (define-key keymap "\e[1;4D" [M-S-left])
    (define-key keymap "\e[1;10A" [M-S-up])
    (define-key keymap "\e[1;10B" [M-S-down])
    (define-key keymap "\e[1;10C" [M-S-right])
    (define-key keymap "\e[1;10D" [M-S-left])
    (define-key keymap "\e[32;2u" [S-space])
    (define-key keymap (kbd "ESC \" 5 R") '[S-return])
    (define-key keymap (kbd "ESC \" 5 r") '[C-return])
    (define-key keymap (kbd "ESC \" 2 R") '[C-S-return])
    (define-key keymap (kbd "ESC \" 5 ;") '[?\C-\;])
    (define-key keymap (kbd "ESC \" 5 :") '[?\C-\:])
    (define-key keymap (kbd "ESC \" 5 ,") '[?\C-\,])
    (define-key keymap (kbd "ESC \" 5 .") '[?\C-\.])
    (define-key keymap (kbd "ESC \" 5 >") '[?\C-\>])
    (define-key keymap (kbd "ESC \" 5 <") '[?\C-\<])
    (define-key keymap (kbd "ESC \" 5 /") '[?\C-\/])
    (define-key keymap (kbd "ESC \" 5 ?") '[?\C-\?])
    (define-key keymap (kbd "ESC \" 5 \'") '[?\C-\'])
    (define-key keymap (kbd "ESC \" 5 \"") '[?\C-\"])
    (define-key keymap (kbd "ESC \" 5 |") '[?\C-|])
    (define-key keymap (kbd "ESC \" 5 \\") '[?\C-\\])
    (define-key keymap (kbd "ESC \" 5 t") '[C-tab])
    (define-key keymap (kbd "ESC \" 5 T") '[C-backtab])
                                        ;    (define-key isearch-mode-map [remap isearch-delete-char]
                                        ;    'isearch-del-char)
    ))
(chopps-add-local-keys)

;; Make S-space insert a space

(global-set-key [S-space] (lambda () (interactive) (insert " ")))

;; ==== Switch to new buffer on C-x 2 or C-x 3 ====
;; Thanks to http://stackoverflow.com/a/6465415/161801.

(global-set-key "\C-x2" (lambda ()
                          (interactive)
                          (split-window-vertically)
                          (other-window 1)))
(global-set-key "\C-x3" (lambda ()
                          (interactive)
                          (split-window-horizontally)
                          (other-window 1)))

;; Use a longer Unicode character for the vertical border
;; http://stackoverflow.com/a/18211568/161801

(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?│))

;; Make M-S-[ and M-S-] *always* move paragraphs

(defun basic-forward-paragraph (&optional arg)
  "Go forward a paragraph"
  (interactive "P") (let ((paragraph-separate "[ 	]*$")
                          (paragraph-start "\\|[ 	]*$"))
                      (forward-paragraph arg)))

(defun basic-backward-paragraph (&optional arg)
  "Go forward a paragraph"
  (interactive "P") (let ((paragraph-separate "[ 	]*$")
                          (paragraph-start "\\|[ 	]*$"))
                      (backward-paragraph arg)))

(global-set-key "\M-{" 'basic-backward-paragraph)
(global-set-key "\M-}" 'basic-forward-paragraph)

;; Make C-U C-SPC work smarter. See
;; http://endlessparentheses.com/faster-pop-to-mark-command.html

(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around
            #'modi/multi-pop-to-mark)

(setq set-mark-command-repeat-pop t)

;; ==== Smarter isearch + occur =====

;; Taken from http://www.emacswiki.org/emacs/OccurFromIsearch. Type M-o when
;; in isearch to get an occur window.

;; TODO Make this happen automatically

(defvar ska-isearch-occur-opened nil)
(defvar ska-isearch-window-configuration nil)

(defun ska-isearch-occur ()
  (interactive)
  (when (fboundp 'occur)
    (setq ska-isearch-occur-opened t)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(defun ska-isearch-maybe-remove-occur-buffer ()
  "Restore window-configuration when quitting isearch.

This function is meant to be used together with a function storing the
window configuration into a variable and together with a setup opening
the occur buffer from within isearch.

This function ...

-  will do nothing if you you did not cancel the search,

- will kill the occur buffer if occur buffer was opened from
  isearch,

- will restore your old window configuration when you saved it in
  `isearch-mode-hook'."

  (interactive)
  (let ((occ-buffer (get-buffer "*Occur*")))
    (when (and ska-isearch-occur-opened
               isearch-mode-end-hook-quit
               (buffer-live-p occ-buffer))
      (kill-buffer occ-buffer)
      (when (and ska-isearch-window-configuration
                 (window-configuration-p (car ska-isearch-window-configuration)))
        (set-window-configuration (car ska-isearch-window-configuration))
        (goto-char (cadr ska-isearch-window-configuration))))))

(add-hook 'isearch-mode-hook
          #'(lambda ()
              (setq ska-isearch-window-configuration
                    (list (current-window-configuration) (point-marker)))))

(add-hook 'isearch-mode-end-hook
          #'(lambda ()
              (ska-isearch-maybe-remove-occur-buffer)
              (setq ska-isearch-occur-opened nil)))

(define-key isearch-mode-map (kbd "M-o") 'ska-isearch-occur)

;; Automatically wrap isearch.
;; http://stackoverflow.com/q/285660/161801

(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; Make some of the isearch keybindings more sane

(define-key isearch-mode-map (kbd "<tab>") 'isearch-complete)

;; Make delete in isearch delete the failed portion completely.
;; http://emacs.stackexchange.com/a/10360/118
;; https://gist.github.com/johnmastro/508fb22a2b4e1ce754e0

(defun isearch-delete-something ()
  "Delete non-matching text or the last character."
  ;; Mostly copied from `isearch-del-char' and Drew's answer on the page above
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setq isearch-string
          (substring isearch-string
                     0
                     (or (isearch-fail-pos) (1- (length isearch-string)))))
    (setq isearch-message
          (mapconcat #'isearch-text-char-description isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

(define-key isearch-mode-map (kbd "DEL") 'isearch-delete-something)
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-delete-something)

;; Better M-SPC behavior

(defun cycle-spacing-with-newline ()
  (interactive)
  (cycle-spacing -1))

(global-set-key (kbd "M-SPC") 'cycle-spacing-with-newline)

;; Make emacsclient open multiple files in separate windows
;; https://emacs.stackexchange.com/questions/3283/make-emacsclient-open-multiple-files-in-separate-windows

(defvar server-visit-files-custom-find:buffer-count)
(defadvice server-visit-files
    (around server-visit-files-custom-find
            activate compile)
  "Maintain a counter of visited files from a single client call."
  (let ((server-visit-files-custom-find:buffer-count 0))
    ad-do-it))
(defun server-visit-hook-custom-find ()
  "Arrange to visit the files from a client call in separate windows."
  (if (zerop server-visit-files-custom-find:buffer-count)
      (progn
        (delete-other-windows)
        (switch-to-buffer (current-buffer)))
    (let ((buffer (current-buffer))
          (window (split-window-sensibly)))
      (switch-to-buffer buffer)
      (balance-windows)))
  (setq server-visit-files-custom-find:buffer-count
        (1+ server-visit-files-custom-find:buffer-count)))
(add-hook 'server-visit-hook 'server-visit-hook-custom-find)

;; ===== Set C-x C-c to do the right thing in emacsclient
;; TODO

;; ===== Make the *scratch* buffer use text mode by default ====

(setq initial-major-mode 'text-mode)

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

;; (setq auto-fill-mode 1)

;; ===== Make Text mode the default mode for new buffers =====

(setq default-major-mode 'text-mode)

;; ;; ===== keyfreq =====
;;
;; (add-to-list 'load-path "~/Documents/keyfreq")
;; (require 'keyfreq)
;; (keyfreq-mode 1)
;; (keyfreq-autosave-mode 1)

;; ===== fido-vertical-mode ====
(if (>= emacs-major-version 28)
    (fido-vertical-mode 1))

;; Use ido just for find-file, because of ido-sort-mtime, which I don't know
;; how to reproduce with fido

(ido-mode "files")

;; ==== Use ssh over tramp ====
;; See http://stackoverflow.com/a/4725727/161801

(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
;; To use this, use C-x C-f C-x C-f (you need to do it twice to get out of
;; ido). Then enter
;; /sudo:root@host[#port]:/path/to/file
;;
;; Note that it says root, but you should use your own password.

;; ;; ==== XTERM title =====
;; ;; Set the XTERM title from within emacs
;; Commented out because it makes emacs really slow
;;
;; (add-to-list 'load-path "~/Documents/xterm-frobs")
;; (add-to-list 'load-path "~/Documents/xterm-title")
;; (when (and (not window-system)
;;            (string-match "^xterm" (getenv "TERM")))
;;   (require 'xterm-title)
;;   (xterm-title-mode 1))
;;
;; (setq frame-title-format "Emacs - %b")  ; Window title
;; (setq icon-title-format "Emacs - %b")   ; Tab titleterm

;; ==== Buffer move ====
;; From http://www.emacswiki.org/cgi-bin/wiki/buffer-move.el
;; (it's in .emacs.d/lisp

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; ==== flyspell ====

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

(defun turn-off-flyspell ()
  "Force flyspell-mode off using a zero arg.  For use in hooks."
  (interactive)
  (flyspell-mode 0))

;; Disable in hexlmode
(add-hook 'hexl-mode-hook 'turn-off-flyspell)

;; Use aspell with flyspell

;; (setq ispell-list-command "list")
;; Make aspell faster. Possibilities, from fastest to slowest, are ultra,
;; fast, normal, bad-spellers (normal is the default). Faster modes have
;; poorer suggestions.
;; (setq-default ispell-extra-args "--sug-mode=ultra")

(setq-default ispell-program-name "hunspell")
(setq ispell-really-hunspell t)

;; ===== Automatically indent with RET =====

(defun newline-and-indent-conditionally ()
  "Acts like newline, unless the point is at the end of the line, then acts
like newline-and-indent"
  (interactive)
  (if (eq (point) (line-beginning-position))
      (newline)
    (newline-and-indent)))

(define-key global-map (kbd "RET") 'newline-and-indent-conditionally)

;; ===== Clear trailing whitespace on save ====

;; This trick makes C-x C-s clear whitespace, *but*, if there was whitespace
;; between the cursor and the end of the line, it puts it back after the save
;; (but still marks the file as saved).  This way, you can save while typing
;; and it won't move the cursor back into the word you just typed if you had a
;; space there, but it always clears whitespace in the saved file.
;; Additionally, this makes C-x C-s clear whitespace even if no changes have
;; been made to the file yet.  To save a file without clearing whitespace, use
;; M-x save-buffer.

(defvar delete-trailing-whitespace-on-save-mode-blacklist '(diff-mode)
  "List of modes to not delete trailing whitespace on save.")

(defun my-save-buffer-dtws (arg)
  "save buffer delete trailing white space, preserve white space before
    point if point is past text"
  (interactive "p")
  (when (not (apply 'derived-mode-p delete-trailing-whitespace-on-save-mode-blacklist))
    (let ((save (when (and (looking-at "[ 	]*$")
                           (looking-back "[ 	]+"
                                         (line-beginning-position) t))
                  (match-string 0))))
      (delete-trailing-whitespace)
      (save-buffer arg)
      (when save
        (insert save)
        (set-buffer-modified-p nil)))))

(global-set-key [remap save-buffer] 'my-save-buffer-dtws)

;; Normally you would just do this:

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ===== Global auto revert mode =====
;; Prevents emacs from constantly bugging about reverting files when you
;; checkout a different git branch.

(global-auto-revert-mode 1)

;; ===== Use four spaces instead of tabs ====

(setq c-basic-offset 4)
(setq tab-width 4)
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
;; (setq-default indent-line-function 'insert-tab)
;; (setq indent-line-function 'insert-tab)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76
                        80))
(setq-default py-indent-offset 4)
(setq-default python-indent 4)

;; ===== Trailing whitespace ======

;; (setq-default highlight-trailing-whitespace)
;; (global-whitespace-mode)

;; ===== Highlight Marked Text =====

(setq-default transient-mark-mode t)

;; ===== Abbreviations =====

(define-abbrev global-abbrev-table "Ondrej" "Ondřej")
(define-abbrev global-abbrev-table "Certik" "Čertík")
(define-abbrev global-abbrev-table "Ondrej's" "Ondřej's")
(define-abbrev global-abbrev-table "Certik's" "Čertík's")

;; ===== Flymake for tex-mode ====

;; flymake-mode for tex uses texify by default, which only works in Windows (miktex)

;; ;; Borrowed from https://github.com/MassimoLauria/dotemacs/blob/master/init-latex.el
;; (defun init-latex--flymake-setup ()
;;   "Setup flymake for latex using one of the checker available on the system.
;; It either tries \"lacheck\" or \"chktex\"."
;;   (interactive)
;;   (cond ((executable-find "lacheck")
;;          (defun flymake-get-tex-args (file-name)
;;            (list "lacheck" (list file-name))))
;;         ((executable-find "chktex")
;;          (defun flymake-get-tex-args (file-name)
;;            (list "chktex" (list "-q" "-v0" file-name))))
;;         (t nil)))
;;
;; (eval-after-load "flymake" '(init-latex--flymake-setup))
;;
;; (defun my-flymake-show-help ()
;;   (when (get-char-property (point) 'flymake-overlay)
;;     (let ((help (get-char-property (point) 'help-echo)))
;;       (if help (message "%s" help)))))
;;
;; (add-hook 'post-command-hook 'my-flymake-show-help)

;; ===== Enable auto-fill-mode for relevant file types =====

(defun turn-on-auto-fill ()
  "Force auto-fill-mode on. For use in hooks."
  (interactive)
  (auto-fill-mode 1))

(defun turn-off-auto-fill ()
  "Force auto-fill-mode off. For us in hooks."
  (interactive)
  (auto-fill-mode -1))

(add-hook 'message-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill) ; All programming languages
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

(add-hook 'makefile-mode-hook 'turn-off-auto-fill)
(add-hook 'css-mode-hook 'turn-off-auto-fill)

;; Always softwrap at word boundaries
(global-visual-line-mode t)

;; Prevent autofill from happening in quoted strings. From
;; https://stackoverflow.com/questions/23755506/emacs-fill-mode-for-python-that-doesnt-break-quoted-strings-causing-errors.

;; (defun odd-number-of-single-quotes-this-paragraph-so-far ()
;;   (and (derived-mode-p 'python-mode) (oddp (how-many "'" (save-excursion (backward-paragraph) (point)) (point)))))
;; (defun odd-number-of-double-quotes-this-paragraph-so-far ()
;;   (and (derived-mode-p 'python-mode) (oddp (how-many "\"" (save-excursion (backward-paragraph) (point)) (point)))))
;;
;; (add-to-list 'fill-nobreak-predicate
;;              'odd-number-of-single-quotes-this-paragraph-so-far)
;; (add-to-list 'fill-nobreak-predicate
;;              'odd-number-of-double-quotes-this-paragraph-so-far)

;; (defun my-make-continuation-line-by-fill ()
;;   (when (eq major-mode 'python-mode)
;;     (save-excursion
;;       (forward-line -1)
;;       (end-of-line)
;;       (unless (member (char-before) (list 92 32))
;;         (insert-and-inherit " ")
;;         (unless (eq (char-after) ?\\)
;;           (insert-and-inherit "\\"))))))
;;
;; (defadvice do-auto-fill (after my-make-continuation-line-by-fill activate)(my-make-continuation-line-by-fill))
;;
;; (ad-activate 'do-auto-fill)

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
;; switches buffers.  Figure out how to make it not switch.  See
;; https://stackoverflow.com/questions/11532149/emacs-make-custom-scrolling-function-follow-mouse-but-not-change-keyboard-focus.

(setq mouse-wheel-follow-mouse 't)

(defvar alternating-scroll-down-next t)
(defvar alternating-scroll-up-next t)

(defun alternating-scroll-down-line ()
  (interactive "@")
  (when alternating-scroll-down-next
    ;; (run-hook-with-args 'window-scroll-functions)
    (scroll-down-line))
  (setq alternating-scroll-down-next (not alternating-scroll-down-next)))

(defun alternating-scroll-up-line ()
  (interactive "@")
  (when alternating-scroll-up-next
    ;; (run-hook-with-args 'window-scroll-functions)
    (scroll-up-line))
  (setq alternating-scroll-up-next (not alternating-scroll-up-next)))

;; The (interactive "@") makes this scroll the window under the mouse instead
;; of the one where the cursor currently is.
(defun scroll-up-line-this-window ()
  (interactive "@")
  (scroll-up-line))

(defun scroll-down-line-this-window ()
  (interactive "@")
  (scroll-down-line))

;; This is no longer necessary in emacs 28.1
;; (global-set-key (kbd "<mouse-4>") 'alternating-scroll-down-line)
;; (global-set-key (kbd "<mouse-5>") 'alternating-scroll-up-line)

(global-set-key (kbd "<mouse-4>") 'scroll-down-line-this-window)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line-this-window)

;; Make mouse 2 (three finger click in iTerm2) do a yank. The default doesn't
;; work in the terminal emacs.

;; (global-set-key (kbd "<mouse-3>") 'mouse-yank-at-click)

;; Make clicking on line numbers work.

(global-set-key (kbd "<left-margin> <mouse-1>") 'mouse-set-point)
;; TODO: This doesn't actually work
(global-set-key (kbd "<left-margin> <mouse-movement>") 'mouse-set-region)
(global-set-key (kbd "<left-margin> <mouse-4>") 'alternating-scroll-down-line)
;; (global-set-key (kbd "<left-margin> <mouse-5>") 'alternating-scroll-up-line)
;; (global-set-key (kbd "<mode-line> <mouse-4>") 'alternating-scroll-down-line)
;; (global-set-key (kbd "<mode-line> <mouse-5>") 'alternating-scroll-up-line)
(global-set-key (kbd "<left-margin> <mouse-5>") 'scroll-up-line)
(global-set-key (kbd "<mode-line> <mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mode-line> <mouse-5>") 'scroll-up-line)


;; Use emacsclient as a mergetool
;; Code from https://www.reddit.com/r/emacs/comments/4j4rle/i_figure_it_out_how_to_use_emacsclient_as_gits/

(defun my/emerge (local remote base output)
  (emerge-files-with-ancestor nil local remote base output nil 'my/emerge--close-current-frame))

(defun my/emerge--close-current-frame ()
  "Close the current frame, duh"
  ;; This is simple enough to be a lamdba, however, that would make
  ;; EMERGE-FILES-WITH-ANCESTOR look ugly, I prefer keeping
  ;; arguments in one line, if possible.
  (delete-frame (selected-frame)))

;; Makes scrolling keep the cursor in the same position on the screen. See http://superuser.com/q/184340/39697

(setq scroll-preserve-screen-position t)

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

;; ===== Turn on flymake-mode ====

;; We don't use this any more, instead, we use flycheck (which comes from
;; use-package).

;; (add-to-list 'load-path "~/Documents/flymake-easy")
;;
;; (add-hook 'c-mode-common-hook 'turn-on-flymake)
;; (add-hook 'latex-mode-hook 'turn-on-flymake)
;; (add-hook 'LaTeX-mode-hook 'turn-on-flymake)
;; ;(add-hook 'python-mode-hook 'turn-on-flymake)
;; (defun turn-on-flymake ()
;;   "Force flymake-mode on. For use in hooks."
;;   (interactive)
;;   (flymake-mode 1))
;;
;; (add-hook 'c-mode-common-hook 'flymake-keyboard-shortcuts)
;; (add-hook 'latex-mode-hook 'flymake-keyboard-shortcuts)
;; (add-hook 'LaTeX-mode-hook 'flymake-keyboard-shortcuts)
;; (add-hook 'python-mode-hook 'flymake-keyboard-shortcuts)
;; (defun flymake-keyboard-shortcuts ()
;;   "Add keyboard shortcuts for flymake goto next/prev error."
;;   (interactive)
;;   (local-set-key "\M-n" 'flymake-goto-next-error)
;;   (local-set-key "\M-p" 'flymake-goto-prev-error))
;;
;; (add-to-list 'load-path "~/Documents/flymake-python-pyflakes")
;; (require 'flymake-python-pyflakes)
;; (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)


;; ==== Use hexl mode for binary files ====

;; http://emacs.stackexchange.com/a/10297/118

(defun buffer-binary-p (&optional buffer)
  "Return whether BUFFER or the current buffer is binary.

A binary buffer is defined as containing at least on null byte.

Returns either nil, or the position of the first null byte."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (search-forward (string ?\x00) nil t 1))))

(defun hexl-if-binary ()
  "If `hexl-mode' is not already active, and the current buffer
is binary, activate `hexl-mode'."
  (interactive)
  (unless (eq major-mode 'hexl-mode)
    (when (buffer-binary-p)
      (hexl-mode))))

(add-hook 'find-file-hooks 'hexl-if-binary)
;; (add-to-list 'magic-fallback-mode-alist '(buffer-binary-p . hexl-mode) t)

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

;; ;; https://gitlab.com/python-mode-devs/python-mode
;; (setq py-install-directory "~/Documents/python-mode/")
;; (add-to-list 'load-path py-install-directory)
;; (require 'python-mode)

;; ;; ===== ipython ====
;; ;; Note, this is linked to in ~/.emacs.d/lisp from the ipython git repo, not
;; ;; the dotfiles repo
;;
;; (require 'ipython)

;; The following lazily enables ropemacs (and only loads pymacs with it).
;; Use C-x p l to load ropemacs.  Uncomment the stuff below it to always load them (slow).

;; ==== python.el ====

;; We use a fork with a branch that uses less stupid indentation for
;; continuation lines.
;; (add-to-list 'load-path "~/Documents/python.el")
;; (require 'python)

;; As long as we use this fork of python.el, we have to avoid electric mode in
;; Python mode (assumedly this is fixed in the version that comes with emacs)

(add-hook 'python-hook (electric-indent-mode 0))

(define-derived-mode xonsh-mode python-mode "Xonsh Mode"
  "A mode for .xsh files.")

(add-to-list 'auto-mode-alist '("\\.xsh\\'" . xonsh-mode))


;; Common files that should use conf-mode (ini files)

(add-to-list 'auto-mode-alist '(".coveragerc" . conf-mode))

;; TODO: instead of changing the source to avoid paren indentation, change it
;; here using defadvice.  See http://stackoverflow.com/a/4150438/161801.

;; Require emacs trunk python.el
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'imenu-create-index-function)
;;                  #'python-imenu-create-index)))

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

;; (add-to-list 'load-path "~/Documents/mmm-mode")
;; (if (locate-library "mmm-auto")
;;         (autoload 'mmm-auto "mmm-auto" "Start mmm mode" t))
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)
;;
;; ;; Make Python docstrings use rst-mode.  See
;; ;; http://stackoverflow.com/q/15493342/161801.  Commented out because there
;; ;; are too many cases where code is rendered as docstrings
;;
;; (mmm-add-classes
;;  '((python-rst
;;     :submode rst-mode
;;     :front "[ru]?\"\"\""
;;     :back "\"\"\""
;;     :include-front nil
;;     :include-back nil
;;     :end-not-begin t
;;     :face mmm-code-submode-face
;;     :)))
;; (mmm-add-mode-ext-class 'python-mode nil 'python-rst)

;; ==== doctest-mode ====
;; Note, this needs to go below mmm-mode above.

;; (add-to-list 'load-path "~/Documents/python-mode/python-mode")
;; (add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))
;; (autoload 'doctest-mode "doctest-mode" "doctest mode" t)
;; (autoload 'doctest-register-mmm-classes "doctest-mode")
;; (doctest-register-mmm-classes t t)

;; ;; ==== direx ====
;; ;; Required for Python imenu direx below
;;
;; (add-to-list 'load-path "~/Documents/direx-el")
;; (autoload 'direx "direx" t)
;; (push '(direx:direx-mode :position left :width 25 :dedicated t)
;;       popwin:special-display-config)
;; (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

;; ==== Emacs Jedi Direx ====
;; A nice imenu for Python

;; Commented out because it is slow to load, and I don't use it.

;; (add-to-list 'load-path "~/Documents/emacs-jedi-direx")
;; (require 'jedi-direx)
;; (eval-after-load "python"
;;   '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))

;; ===== Scroll bars ======

;; This is related to auto-complete-mode (same developer)

;; (add-to-list 'load-path "~/Documents/yascroll-el")
;; (require 'cl)
;; (require 'yascroll)
;; (global-yascroll-bar-mode 1)

;; ;; Discover mode

;; ;; http://www.masteringemacs.org/articles/2013/12/21/discoverel-discover-emacs-context-menus/

;; (require 'discover)
;; (global-discover-mode 1)

;; ==== rectangle-mark-mode ====

;; Because C-x SPC is already used by ace-jump-mode

(global-set-key (kbd "C-x r C-SPC") 'rectangle-mark-mode)

;; ==== RTF mode ====

(autoload 'rtf-mode "rtf-mode" "RTF mode" t)
(add-to-list 'auto-mode-alist
             '("\\.rtf$" . rtf-mode))

;; ==== ntcmd =====
;; Mode for editing .bat files (Windows batch files)
(autoload 'ntcmd-mode "ntcmd" "ntcmd mode" t)
(add-to-list 'auto-mode-alist
             '("\\.bat$" . ntcmd-mode))

;; ==== Predictive ====

;; (add-to-list 'load-path "~/Documents/predictive")
;; (require 'predictive)

;; ;; ==== Highlight indentation =====
;;
;; (require 'highlight-indentation)
;;
;; (add-hook 'prog-mode-hook 'highlight-indentation)

;; ==== goto-last-change.el
;; http://www.emacswiki.org/emacs/download/goto-last-change.el
;; http://superuser.com/a/184402/39697

;; (require 'goto-last-change)

;; ;; goto-last-change doesn't work with undo-tree, but the following is a pretty
;; ;; good equivalent.
;; (defun undo-redo ()
;;   (interactive)
;;   (progn
;;     (undo-tree-undo)
;;     (undo-tree-redo)))
;; (global-set-key (kbd "C-x C-\\") 'undo-redo)

;; (require 'goto-chg)
;; (global-set-key [(control ?.)] 'goto-last-change)
;; (global-set-key (kbd "C-,") 'goto-last-change-reverse)

;; ==== Tabbar mode ====

;; Disabled because I couldn't figure out how to make it do what I want
;; (always show all files by filename)

;; (add-to-list 'load-path "~/Documents/tabbar")
;; (require 'tabbar)

;; ;; ===== E2WM (Emacs Window Manager) ====
;; ;; For its imenu implementation
;;
;; (add-to-list 'load-path "~/Documents/emacs-window-layout")
;; (add-to-list 'load-path "~/Documents/emacs-window-manager")
;; (require 'e2wm)

;; ===== Keyboard macros =====

;; Define a keyboard macro with F3 and use it with F4. To save it, use C-x C-k
;; n and give it a name. Then use M-x insert-kbd-macro <name> here to save it

(fset 'fix\ numpy\ stubs
      (kmacro-lambda-form [?\C-s ?f ?r ?o ?m ?  ?. ?\C-m ?\C-a ?\C-k ?\C-? ?\C-s ?r ?e ?t ?u ?r ?n ?\C-m ?\C-\[ ?f ?\C-\[ ?b ?n ?p ?.] 0 "%d"))

;; =======================================
;; ===== Values set by M-x customize =====

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(aggressive-indent-excluded-modes '(markdown-mode python-mode makefile-mode c-mode c++-mode sass-mode))
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(auto-save-list-file-prefix "~/.emacs.d/autosave/")
 '(colon-double-space t)
 '(command-log-mode-auto-show t)
 '(comment-column 0)
 '(comment-empty-lines ''eol)
 '(completion-ignore-case t t)
 '(completion-ignored-extensions
   '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".h5py" ".h5" ".lprof" ".DS_Store" "__pycache__/"))
 '(cua-enable-cua-keys nil)
 '(cua-enable-modeline-indications t)
 '(cua-enable-region-auto-help nil)
 '(cua-keep-region-after-copy t)
 '(cua-mode nil nil (cua-base))
 '(cua-paste-pop-rotate-temporarily t)
 '(custom-enabled-themes '(1am))
 '(custom-safe-themes
   '("d4e701b16e06a089206e2e8c7f9ee96f149168ab193fbc57f5f96c55bcfd8dec" "7f238f10b769737a46c399029194a65dfdc910eb45047994e79bf3dd8c9ccea4" "fcc32f8f8de75a0b2c69c295325aff848cac4dcf0eff4ebf7e572a3b2c4f4857" "e35eaec838eb8b9c655f15e5a660224c5b3cf6c270246b6f74d8963ce98230c9" "18eeb480ddfa7d52736f6d3c7b47bba34c703aa3d79b675b16fc25d31dbb953e" "6ec3066c030a0e14d5178c0d9cba9c72b3ccb65e6cf02f69404b3fb60d0f30ec" "573edaffdad0b083a266767d80cab7bb55e4e677dbf1f5384a92909e5ec6b428" "2b84353a00aa1d56d2b226ca4b7e20979da8fc360adce5026d69507e60893131" "e03b7730bda16f97e2be4d95d6d4d3fd18263521326c8d5f81251b53515ef8d3" "d62640f79a6d3ac96447306d4db8a533308eb396d2a27f8f45e8bc7947a7306d" "eadc78d2eee6898c46392fdfea01494f9cc32032dcca1bf28420934ca28ab82a" "b8e71c08603747445ce5ed74a4ced4c7c04a1372ea26d8cb95fa1f0529b16e5c" "8518329e6248e61085b4bef4dac5d4f62076d2b177b48837795d1237b513e4bb" "8c4ff5658c49df50b212ea8429db66030e4dd2ec6d378d862a114a498ebc91ff" "919f4b755c69a7a6882d7c06d746362b81047e9d071cf088e1c33e8fc3cc0a00" "faff902b7e3d1bc88a2732d21f143797a73af75e85eb3e6425903a1c19fbde4d" "c6119d58d6fb3a4fa0bd6f7a2f5fd7d373a8c018cd052d5132666af106eaccd7" "4af3774f07329c90391689fbc875b61fe45a12594320e13b6ded22fad1a7b777" "f9fc0dbdab679e61faf1f41e17cdb4563db6c2f94a4a410b5af0508b357e3eb4" "b9a618f3d1db7253cc64cc50bc20717e26bbf4ad45e70c0ef88b8d49c3339a3c" "9d4cd0c4ecf753ba2008438680b2c168009b6c9477ed834cd1adaac5658c9ad9" "4d9935a887e72e35114fc4b4b702fd616bf6d0f6a433a1328add7d168107c265" "4fac3b63166615817cb1913e189d675189191688dac7cdc8dd2516db7c44c7d1" "9ac84d6ade4d20d9a079af263c4804b547b1e179db7e58b551dfef8c736d9f5f" "2fece19cf36c22fef90e432abb3c3bb5249fb3b15c8dcc7ae3e374e30e44f53d" "364149ff1ec800e7abeeb3d17d69fe255ed33a9092f8c09cf197daf1ba68b4c9" "91a352b95d37b2e56c8c7144dd0afd6e98f8f8362ba4887671a5519f6ae1ac35" "7ef4c58872c820f1512f4880c968b46fb7688c3eb7b6cada6e8e54e6f1d48eb8" "9fb46c19b162a0d72f58b68eb4623bb932fb71c89cfb11a8c3ab9b67902e1896" "397cfed5740b0c02bd4979f67785b12c9099398804dc473b31f39074d8a7bde9" "29fba31774979ef63d03963e230a475d02e144dae99d9cdee31bdaa48ea5cdc5" "b49c9ca043d1e9124a1dd81ab98530758618ca0e15b5f24feca7d5490c1deea2" "f52b1864cc0b8fdfbfd5fe8823a4b776e3becd57e0834318d714e21a6696b721" "7c26d530d061dd6549df8eee2a08c70f4438fa481f49673b37106e52bb0c14f7" "384f802ab7a5457b9b2688bac71bf9e7fc116477c0b42f8fef88e53e5b34ec08" "38d61fb5db711ec252c9f684357baafabc6b5490949844c3fe7460642dda050f" "fb81f47208097b78d4846833b48d2a505bcdaca75a83c5015e633d828dd5684b" "5b3815bb0f47c6e7cdd274ff8966d7cc28cfdfc59cfab275a861800a0a1e3685" "9ed4115588b5d0c4ee92a49e82c5e09ac79fad1156b355da61847437e76c8443" "9dc27d023016e67746a8300ce7803afc12db3a5f792e0c1149c678ee737cd393" "91c441fdebcb96e2a400099f5f4d1e3f7d0369ba3adfde2db85768fcbe8be36a" "a7a82ab073db1d67d8637f36b0ef00e7676a282d78053272a6ed35d12469b5cd" "12c47811b507f224f67a77980dcc549f480dd07f4833d4897807c8cf0cce1f18" "010ee97548807cda9fffed7987b6cd4411787d0021bcdcb1e6af4e9cb7ba242d" "baff1b8e7a57d2f305ec62a0192b8b5dd8e51dffc07397db995bbc9f44b6260d" "4aeaf5b2344a118525264d9f30fc7449674fe944f1b138eb3ae9316d708012bd" "8fae94af554271592cd9d6fc85e641f39b1c7bf079d7443ea310dff66846e6ea" "1d7d53c6b3c78148188d119d1ba5244ea10641c0dc58f3d039761a7d2894d651" "f65473c79835b5d109e94f56e7bdee90364331ccdc941ca3ffeeba904030bea9" "7f7317df88f49c02f5e92fc6f170649c2c60a708e643aa36945208f77052b8e2" "b88dad94b783f49880065d4165dc8495ce67d9cbb5b173c2c5889768a0a5092c" "64beff7d92ff58e4f36ea72562f6c7af1b64575cc261024e9a796706a4184f1b" "444384e2fb577b2feaa1d40b7623155d15a0daac1bde2101450ce44c06ae3cdf" "c2220bbdf1a461bb2f5db9518115bca588c3a28a54f077eaae33552e7ae5febf" "59b01ffe8606b179744cd932f7c6c325f10ecd6c8a4aad9f98fb7893a62cdc81" "fa3c930770e321864b5411cf4e753e62c67c02821f52f46330b075a873753dbd" "c823b85afed4fa75395f4f8f79dcd6f86543e69e46c513719ac78167490394a0" "dbbe1d9d8e0b19a2fcc730cb2c553bcabe6b9703a3c963f076f3ac080d2ebf7a" "2724e4d5017b2d5d47701d67c8261ad4e3860f5f496701cfab8bacd5b3989071" "5f881dabbfd8d93218e8e353a388399eea5e5c0174f39f5d6d6ee46eaaf39f13" "d61ca2a6102b47feeeccd0ef47f63fdeca3e193c1c7d6aed3d14bec7960c0ad7" "2fbe6f5c2ccb71cf888b2d9511af2d73d55b6f61d8587ddd22ac92c4cb159a5d" "2c945467e47365b6a721e3d91b7045f66fcd059c8e9611e8b1924a8aba4482f7" "e0b569c74dfcdb9c9cd0694a4f5580de8c77436b74c055197d3bdf856f50a969" "7dad3bf7c48c5250bb91dc70ac35a2899aea38ffff1731eaee759818737eb5a1" "793e990947d64992a749cb823fbc1bddaa56f8cf558c6fb4d0c679bc28b10015" "dced14ee177920d2ebac9b8e6af3dafc24bd00dfa21b81df4753a0a6dc0ffdc3" "f2f58047008edf991421378f85cb5c74c3b104153a179b67ac4b6e6b0f917fac" "a5c3290ffec11c14737f708a5a80e11533dba0214405d7a5b912f92bcd5ebf88" "f1f3e0873563e0929f45e5b832df4357208c0a99550b801d85170024afcf5293" "f2ee729c4f820f5f6a5c6a10d71ea540cff7d4faef82e80bfc98bb5217d91334" "33b831dbbb06d68cd6a085efd476d31a8161200e848abc11124420f44e94a0e5" "e7ae58a07d42168857c6ec4e8e43146fc363c4834156e7655512b09c394cbe0c" "b75f34d7bb2b240e0bff8e7cee2bc9d155b0ae3ea4f205ec219718428f21eacc" "c0f296362f1eb985e970e61a5475c892faa44ca245c59d087ee9b36b76b043e8" "e48dca04d05f441f1e46c758f01d9ae70d83eca04e6ca8ddd0d82f465ba17a34" "8276ddcf003d6389bdcfb5c2dd8fcdeae1a4ef84f82b68aa4eb3a39ced99229f" "6f7b04d2d3023459b4c8d5cc332a6f0011653da6c44c67ec7f9defa61d9d9702" "3afedf94b282d97db1bba88737d54997e8c3b5aca39af60ad8e0c99280f45468" "05b1b8e7a5b4ed97a9f3d90f80f849da026783d4f68030c14d1f17667382535f" "f71ecd126c6ba71d400e5252f5b83cb463a93da0376deeacf0e19029bde7da8b" "f1cff7b73cccc579c30b880c9f7f07672f4b61e65302f27d052d86dc61b2d15c" "deebfa6364832155c50b5d01a6203a1c0a947901af97ef0bb647bd63fc9986c8" "8dda88444892b832a80a59b0697d8e49c2a6f3487a5f38964d86fbdf3639363b" "94a0edf18f016a4be5b4c4f6207c048ecb6a7c2388578b361d7f405121dd5ddc" "09277acdebe128259f49b20e0cac0f167fc5d21bc0055e011afcbaedf4eee63b" "c1fc09025d628eea1b3fbbc51ca1297c61c689cc6c091a1be187fe1c075a4530" "ad1761ad53fea37fb45993ee3f8d655f52eee8249ea1c91195de702abc88f6b9" "76e393a1b358819802bd21c236d9dff8d2a8fe8295755227c0d85132aeba17c3" "3a930c799b42da0475035d545e34c290c50747e9bd327f007d6a6e2b7e32e8f3" "052e1cc4fe7eb51c63e1096bae6ba1552e2c6ef8b047077a3c1d6d3a5474470a" "2cdbd9a58bad15f1a1d2cbcdb65411d2a8ffe7277caee986f9f7e85ebf9f685d" "984c105aedff7f5d8bee9aae2ad70b7b547957b6a62665bc6de6af0488df6c09" "adf1beba5dd1dcec61c3e080cf0368efbf5e9045b7767e18b27fb9af8682ebc3" "2f54b8f1d15fd066721eb4249ec20cd675b94b34e1fc2721e76b7d5c8d61d2c3" "6f16b05a09588b3e6fcfd30ec258b02a729e581ebb674d2929ff517210694af5" "f81c3986426da442140cc03ff40b104263c6cee2d5b6660394bd3a59f3de7e80" "d97477c58646efd83b364b4614645b42645b12b4c2616ab451b4bcb55200c84d" "58b8232a96b1a6f6b3153ff2038a3c94b37230848c2094e902d555a414c4c293" "dc032b7a76890122b8f67bfccdcdd37cdf4bb505ee8025810535e188ff41b5a9" "b6820d3c35f05f9d4e72b48933141dbcaca5c3dcd04fee7aa3a14bf916d96f21" "94b148d58fe160cb829065dc32c40a254028fc2f6bded2ebcf83c844eb23dc49" "4935aebec6ce9b6b8345e1be34be1c2e321c6f283ddb2bd64f028aa7ce772b8c" "e81394a3cb1468b4e27c6bfd517f4d525694280acf0610ddefea902b9a558f81" "657bb4173efa15dbee1b3406bcacd9e8311983d9870506fad653a07fbe8fb7d2" "b80cd50a6e695872ff71c825dc7d41f457993e9e693ea3931141a02faf7fe646" "079894f67d56b12cfeaf3ae929dc7b486fcfac84127486e581de6e91462596ff" "7cc2ed6fc6b1d9ef1f245745ded639d934e28bb55f70add829ff6bc4bf337da2" default))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(custom-unlispify-tag-names nil)
 '(delete-selection-mode t)
 '(desktop-locals-to-save
   '(desktop-locals-to-save truncate-lines case-fold-search case-replace fill-column overwrite-mode change-log-default-name line-number-mode column-number-mode size-indication-mode buffer-file-coding-system indent-tabs-mode tab-width indicate-buffer-boundaries indicate-empty-lines show-trailing-whitespace buffer-undo-list))
 '(desktop-save-mode nil)
 '(diff-switches "-u")
 '(display-line-numbers t)
 '(doctest-optionflags '("NORMALIZE_WHITESPACE" "ELLIPSIS"))
 '(flycheck-disabled-checkers '(python-flake8 python-pylint proselint))
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-lazy-changes-threshold 10)
 '(flyspell-lazy-idle-seconds 0)
 '(flyspell-lazy-less-feedback t)
 '(flyspell-lazy-mode t)
 '(flyspell-lazy-size-threshold 5)
 '(flyspell-lazy-use-flyspell-word nil)
 '(flyspell-lazy-window-idle-seconds 3)
 '(flyspell-prog-text-faces
   '(font-lock-string-face font-lock-comment-face font-lock-doc-face markdown-pre-face))
 '(git-gutter:update-interval 2)
 '(global-aggressive-indent-mode t)
 '(global-flycheck-mode t nil (flycheck))
 '(global-linum-mode nil)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(highlight-symbol-idle-delay 0)
 '(isearch-allow-motion t)
 '(isearch-allow-scroll 'unlimited)
 '(isearchp-drop-mismatch 'replace-last)
 '(ispell-highlight-face 'flyspell-incorrect)
 '(ispell-program-name "hunspell")
 '(ispell-silently-savep t)
 '(ispell-use-ptys-p t)
 '(jedi:complete-on-dot t)
 '(jedi:environment-root nil)
 '(jedi:imenu-create-index-function 'jedi:create-flat-imenu-index)
 '(jedi:install-imenu nil)
 '(jedi:server-command
   `("~/Documents/emacs-jedi/env/bin/python" ,(expand-file-name "~/Documents/emacs-jedi/jediepcserver.py")))
 '(jedi:use-shortcuts t)
 '(kill-do-not-save-duplicates t)
 '(large-file-warning-threshold nil)
 '(latex/view-after-compile nil)
 '(linum-format "%d⎢")
 '(magit-diff-refine-hunk 'all)
 '(mark-even-if-inactive nil)
 '(markdown-gfm-use-electric-backquote nil)
 '(menu-bar-mode nil)
 '(mouse-wheel-scroll-amount '(1))
 '(next-screen-context-lines 10)
 '(package-selected-packages
   '(ace-window web-mode jinja2-mode git-gutter highlight-symbol avy flycheck-pyflakes use-package flycheck))
 '(python-fill-docstring-style 'onetwo)
 '(python-indent-guess-indent-offset nil)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(require-final-newline t)
 '(safe-local-variable-values '((flycheck-mode) (encoding . utf-8)))
 '(save-place-mode t)
 '(scroll-step 1)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-paren-style 'mixed)
 '(show-trailing-whitespace t)
 '(smart-comment-end-action 'smart-comment-end)
 '(sml-modeline-len 17)
 '(sml-modeline-mode t)
 '(speedbar-visiting-tag-hook '(speedbar-highlight-one-tag-line speedbar-recenter))
 '(split-height-threshold 80)
 '(split-width-threshold 50)
 '(split-window-keep-point t)
 '(tab-bar-show 1)
 '(tab-width 4)
 '(tags-case-fold-search t)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo-tree/")))
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
 '(vr/default-regexp-modifiers '(:I t :M t :S nil :U t))
 '(vr/engine 'python)
 '(vr/match-separator-use-custom-face t)
 '(web-mode-enable-css-colorization t)
 '(window-combination-limit nil)
 '(window-combination-resize t)
 '(xterm-extra-capabilities '(modifyOtherKeys))
 '(xterm-mouse-mode t)
 '(xterm-set-window-title t)
 '(xterm-title-frame-title-format "%b")
 '(xterm-title-mode t)
 '(yascroll:delay-to-hide nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-fold-unfolded-face ((t nil)))
 '(sml-modeline-end-face ((t (:background "black" :foreground "white"))))
 '(sml-modeline-vis-face ((t (:inherit yascroll:thumb-text-area))))
 '(yascroll:thumb-text-area ((t (:background "slateblue" :foreground "white")))))
