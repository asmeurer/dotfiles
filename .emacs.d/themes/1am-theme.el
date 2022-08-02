(deftheme 1am
  "Theme meant for green text on semi-transparent black background. Loosely based on XCode's midnight theme (hence the name).")

(custom-theme-set-faces
 '1am
 ;; Use this instead for GUI emacs.
 ;; TODO: Make this switch automatically.
 ;; '(default ((t (:inherit nil :stipple nil :background "black" :foreground "#00f900" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "Menlo"))))
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "Menlo"))))
 '(region ((t (:background "lightgoldenrod2" :extend t :distant-foreground "black"))))
 '(highlight-indent-face ((t (:background "color-233"))))
 '(font-lock-string-face ((t (:foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "white"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground
                                    "brightyellow"))))
 '(font-lock-keyword-face ((t (:foreground "purple"))))
 '(font-lock-builtin-face ((t (:foreground "dark slate blue"))))
 '(font-lock-constant-face ((t (:foreground "dark cyan"))))
 '(font-lock-function-name-face ((t (:foreground "Blue1" :weight bold))))
 '(font-lock-type-face ((t (:foreground "ForestGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "sienna"))))
 '(minibuffer-prompt ((t (:foreground "red"))))
 '(trailing-whitespace ((t (:background "indianred4"))))
 '(error ((t (:foreground "darkorange3"))))
 '(flycheck-error ((t (:foreground "LightPink"))))
 '(flycheck-warning ((t (:inherit warning))))
 '(flymake-errline ((t (:foreground "LightPink" :underline "red"))))
 '(flymake-warnline ((t (:inherit warning))))
 '(rst-level-1 ((t (:foreground "grey85"))))
 '(rst-level-2 ((t (:foreground "grey78"))))
 '(rst-level-3 ((t (:foreground "grey71"))))
 '(rst-level-1-face ((t (:foreground "cyan"))) t)
 '(rst-level-2-face ((t (:inherit rst-level-1-face :foreground "yellow"))) t)
 '(rst-level-3-face ((t (:inherit rst-level-1-face))) t)
 '(rst-level-4-face ((t (:inherit rst-level-1-face))) t)
 '(rst-level-5-face ((t (:inherit rst-level-1-face))) t)
 '(rst-level-6-face ((t (:inherit rst-level-1-face))) t)
 '(mmm-default-submode-face ((t nil)))
 '(ace-jump-face-background ((t (:foreground "gray"))) t)
 '(ace-jump-face-foreground ((t (:foreground "magenta"))) t)
 '(diff-added ((t (:inherit diff-changed))))
 '(helm-selection ((t (:foreground "brightwhite"))))
 '(ido-first-match ((t (:underline t :weight bold))))
 '(match ((t (:inherit isearch))))
 '(show-paren-match ((t (:background "blue"))))
 '(vr/group-0 ((t (:background "darkred" :foreground "black"))))
 '(vr/group-1 ((t (:background "deep pink" :foreground "black"))))
 '(vr/group-2 ((t (:background "blueviolet" :foreground "white"))))
 '(vr/match-0 ((t (:background "navyblue" :foreground "white"))))
 '(vr/match-1 ((t (:background "blue3" :foreground "white"))))
 '(font-latex-sectioning-5-face ((t (:foreground "blue1" :weight bold))))
 '(highlight-symbol-face ((t (:background "grey10"))))
 )

(provide-theme '1am)
