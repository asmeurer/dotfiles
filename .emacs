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

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/Documents/use-package/"))

;; Make packages always auto-install
(setq use-package-always-ensure t)
;; It doesn't seem to work for some reason, so we use :ensure t below.

;; Install various packages

;; ==== flycheck ====

(use-package flycheck
  :bind
  (("M-n" . flycheck-next-error)
   ("M-p" . flycheck-previous-error))
  :custom
  (flycheck-disabled-checkers '(python-flake8 python-pylint)))

;; ===== flycheck-pyflakes ======

(use-package flycheck-pyflakes)


;; ;; ===== ido-vertical-mode =====
;;
;; ;; This used to be done by this
;; ;; Display ido results vertically, rather than horizontally
;; ;; (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;; ;; (defun ido-disable-line-trucation () (set (make-local-variable
;; ;;                                            'truncate-lines) nil))
;; ;; (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)
;;
;; Commented out because it's installed by use-package
;;
;; (add-to-list 'load-path "~/Documents/ido-vertical-mode.el")
;; (require 'ido-vertical-mode)
;; ;(ido-mode 1)
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1))


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
   (undo-tree-history-directory-alist (quote ((".*" . "/Users/aaronmeurer/.emacs.d/undo-tree/"))))))

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

;; ==== pcre2el (Perl compatible regular expressions) ====

;; Use pcre-query-replace-regexp

;; (add-to-list 'load-path "~/Documents/pcre2el")
;; (require 'pcre2el)
(use-package pcre2el
  :config
  (pcre-mode 1))

;; ==== bug-hunter ====
(use-package bug-hunter)

;; ==== cython-mode ====
(use-package cython-mode)

;; ==== ido-sort-mtime ====
(use-package ido-sort-mtime
  :config
  (ido-sort-mtime-mode 1))

;; ==== auctex ====
(use-package auctex)

;; ==== cask-mode ====
;; We don't use cask any more but cask-mode can still be useful for editing
;; the Cask files
(use-package cask-mode)

;; ==== latex-extra ====

(add-hook 'LaTeX-mode-hook #'latex-extra-mode)

;; ==== Predictive ====

;; (add-to-list 'load-path "~/Documents/predictive")
;; (require 'predictive)

;; ===== isearch+ =====

;; This cannot be installed with use-package because it is on the wiki, and
;; melpa no longer lists wiki packages.
(add-to-list 'load-path "~/Documents/isearch-plus")

(eval-after-load "isearch" '(require 'isearch+))

;; Disable bell ringing in isearch+
(setq isearchp-ring-bell-function nil)

;; Visual regexp

(add-to-list 'load-path "~/Documents/visual-regexp.el") ;; if the files are not already in the load path
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

;; if the files are not already in the load path
(add-to-list 'load-path "~/Documents/visual-regexp-steroids.el/")
(require 'visual-regexp-steroids)
;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
(define-key global-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key global-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s

;; ===== Smart comment =====

(global-set-key (kbd "M-;") 'smart-comment)

;; ===== expand-region =====

(add-to-list 'load-path "~/Documents/expand-region.el")
(autoload 'er/expand-region "expand-region")
(global-set-key (kbd "M-=") 'er/expand-region)

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

;; =======================================
;; ===== Values set by M-x customize =====

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(aggressive-indent-excluded-modes (quote (markdown-mode python-mode makefile-mode)))
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(auto-save-list-file-prefix "/Users/aaronmeurer/.emacs.d/autosave/")
 '(colon-double-space t)
 '(comment-column 0)
 '(comment-empty-lines (quote (quote eol)))
 '(cua-enable-cua-keys nil)
 '(cua-enable-modeline-indications t)
 '(cua-enable-region-auto-help nil)
 '(cua-keep-region-after-copy t)
 '(cua-mode nil nil (cua-base))
 '(cua-paste-pop-rotate-temporarily t)
 '(custom-enabled-themes (quote (1am)))
 '(custom-safe-themes
   (quote
    ("d61ca2a6102b47feeeccd0ef47f63fdeca3e193c1c7d6aed3d14bec7960c0ad7" "2fbe6f5c2ccb71cf888b2d9511af2d73d55b6f61d8587ddd22ac92c4cb159a5d" "2c945467e47365b6a721e3d91b7045f66fcd059c8e9611e8b1924a8aba4482f7" "e0b569c74dfcdb9c9cd0694a4f5580de8c77436b74c055197d3bdf856f50a969" "7dad3bf7c48c5250bb91dc70ac35a2899aea38ffff1731eaee759818737eb5a1" "793e990947d64992a749cb823fbc1bddaa56f8cf558c6fb4d0c679bc28b10015" "dced14ee177920d2ebac9b8e6af3dafc24bd00dfa21b81df4753a0a6dc0ffdc3" "f2f58047008edf991421378f85cb5c74c3b104153a179b67ac4b6e6b0f917fac" "a5c3290ffec11c14737f708a5a80e11533dba0214405d7a5b912f92bcd5ebf88" "f1f3e0873563e0929f45e5b832df4357208c0a99550b801d85170024afcf5293" "f2ee729c4f820f5f6a5c6a10d71ea540cff7d4faef82e80bfc98bb5217d91334" "33b831dbbb06d68cd6a085efd476d31a8161200e848abc11124420f44e94a0e5" "e7ae58a07d42168857c6ec4e8e43146fc363c4834156e7655512b09c394cbe0c" "b75f34d7bb2b240e0bff8e7cee2bc9d155b0ae3ea4f205ec219718428f21eacc" "c0f296362f1eb985e970e61a5475c892faa44ca245c59d087ee9b36b76b043e8" "e48dca04d05f441f1e46c758f01d9ae70d83eca04e6ca8ddd0d82f465ba17a34" "8276ddcf003d6389bdcfb5c2dd8fcdeae1a4ef84f82b68aa4eb3a39ced99229f" "6f7b04d2d3023459b4c8d5cc332a6f0011653da6c44c67ec7f9defa61d9d9702" "3afedf94b282d97db1bba88737d54997e8c3b5aca39af60ad8e0c99280f45468" "05b1b8e7a5b4ed97a9f3d90f80f849da026783d4f68030c14d1f17667382535f" "f71ecd126c6ba71d400e5252f5b83cb463a93da0376deeacf0e19029bde7da8b" "f1cff7b73cccc579c30b880c9f7f07672f4b61e65302f27d052d86dc61b2d15c" "deebfa6364832155c50b5d01a6203a1c0a947901af97ef0bb647bd63fc9986c8" "8dda88444892b832a80a59b0697d8e49c2a6f3487a5f38964d86fbdf3639363b" "94a0edf18f016a4be5b4c4f6207c048ecb6a7c2388578b361d7f405121dd5ddc" "09277acdebe128259f49b20e0cac0f167fc5d21bc0055e011afcbaedf4eee63b" "c1fc09025d628eea1b3fbbc51ca1297c61c689cc6c091a1be187fe1c075a4530" "ad1761ad53fea37fb45993ee3f8d655f52eee8249ea1c91195de702abc88f6b9" "76e393a1b358819802bd21c236d9dff8d2a8fe8295755227c0d85132aeba17c3" "3a930c799b42da0475035d545e34c290c50747e9bd327f007d6a6e2b7e32e8f3" "052e1cc4fe7eb51c63e1096bae6ba1552e2c6ef8b047077a3c1d6d3a5474470a" "2cdbd9a58bad15f1a1d2cbcdb65411d2a8ffe7277caee986f9f7e85ebf9f685d" "984c105aedff7f5d8bee9aae2ad70b7b547957b6a62665bc6de6af0488df6c09" "adf1beba5dd1dcec61c3e080cf0368efbf5e9045b7767e18b27fb9af8682ebc3" "2f54b8f1d15fd066721eb4249ec20cd675b94b34e1fc2721e76b7d5c8d61d2c3" "6f16b05a09588b3e6fcfd30ec258b02a729e581ebb674d2929ff517210694af5" "f81c3986426da442140cc03ff40b104263c6cee2d5b6660394bd3a59f3de7e80" "d97477c58646efd83b364b4614645b42645b12b4c2616ab451b4bcb55200c84d" "58b8232a96b1a6f6b3153ff2038a3c94b37230848c2094e902d555a414c4c293" "dc032b7a76890122b8f67bfccdcdd37cdf4bb505ee8025810535e188ff41b5a9" "b6820d3c35f05f9d4e72b48933141dbcaca5c3dcd04fee7aa3a14bf916d96f21" "94b148d58fe160cb829065dc32c40a254028fc2f6bded2ebcf83c844eb23dc49" "4935aebec6ce9b6b8345e1be34be1c2e321c6f283ddb2bd64f028aa7ce772b8c" "e81394a3cb1468b4e27c6bfd517f4d525694280acf0610ddefea902b9a558f81" "657bb4173efa15dbee1b3406bcacd9e8311983d9870506fad653a07fbe8fb7d2" "b80cd50a6e695872ff71c825dc7d41f457993e9e693ea3931141a02faf7fe646" "079894f67d56b12cfeaf3ae929dc7b486fcfac84127486e581de6e91462596ff" "7cc2ed6fc6b1d9ef1f245745ded639d934e28bb55f70add829ff6bc4bf337da2" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(custom-unlispify-tag-names nil)
 '(delete-selection-mode t)
 '(desktop-locals-to-save
   (quote
    (desktop-locals-to-save truncate-lines case-fold-search case-replace fill-column overwrite-mode change-log-default-name line-number-mode column-number-mode size-indication-mode buffer-file-coding-system indent-tabs-mode tab-width indicate-buffer-boundaries indicate-empty-lines show-trailing-whitespace buffer-undo-list)))
 '(desktop-save-mode nil)
 '(diff-switches "-u")
 '(doctest-optionflags (quote ("NORMALIZE_WHITESPACE" "ELLIPSIS")))
 '(flycheck-disabled-checkers (quote (python-flake8 python-pylint)))
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-lazy-changes-threshold 10)
 '(flyspell-lazy-idle-seconds 1)
 '(flyspell-lazy-less-feedback t)
 '(flyspell-lazy-mode t)
 '(flyspell-lazy-size-threshold 5)
 '(flyspell-lazy-use-flyspell-word nil)
 '(flyspell-lazy-window-idle-seconds 3)
 '(global-flycheck-mode t nil (flycheck))
 '(global-linum-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(isearchp-drop-mismatch (quote replace-last))
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-program-name "hunspell" t)
 '(ispell-silently-savep t)
 '(ispell-use-ptys-p t)
 '(jedi:complete-on-dot t t)
 '(jedi:environment-root nil)
 '(jedi:imenu-create-index-function (quote jedi:create-flat-imenu-index) t)
 '(jedi:install-imenu nil t)
 '(jedi:server-command
   (quote
    ("/Users/aaronmeurer/Documents/emacs-jedi/env/bin/python" "/Users/aaronmeurer/Documents/emacs-jedi/jediepcserver.py")))
 '(jedi:use-shortcuts t t)
 '(large-file-warning-threshold nil)
 '(latex/view-after-compile nil)
 '(linum-format "%d⎢")
 '(menu-bar-mode nil)
 '(mouse-wheel-scroll-amount (quote (1)))
 '(next-screen-context-lines 10)
 '(package-selected-packages (quote (flycheck-pyflakes use-package flycheck)))
 '(pcomplete-ignore-case t)
 '(python-fill-docstring-style (quote onetwo))
 '(python-indent-guess-indent-offset nil)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((flycheck-mode) (encoding . utf-8))))
 '(save-place-mode t)
 '(scroll-step 1)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-trailing-whitespace t)
 '(smex-mode t nil (smex))
 '(sml-modeline-len 17)
 '(sml-modeline-mode t)
 '(speedbar-visiting-tag-hook (quote (speedbar-highlight-one-tag-line speedbar-recenter)))
 '(split-height-threshold 80)
 '(split-width-threshold 50)
 '(split-window-keep-point t)
 '(tab-width 4)
 '(tags-case-fold-search t)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote ((".*" . "/Users/aaronmeurer/.emacs.d/undo-tree/"))))
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(vr/default-regexp-modifiers (quote (:I t :M t :S nil :U t)))
 '(vr/match-separator-use-custom-face t)
 '(window-combination-limit nil)
 '(window-combination-resize t)
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
(put 'downcase-region 'disabled nil)
