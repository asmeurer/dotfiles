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

(add-to-list 'load-path "~/Documents/emacs-jedi")
(require 'jedi)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
