(deftheme 1am
  "Theme meant for green text on semi-transparent black background. Loosely based on XCode's midnight theme (hence the name).")

(custom-theme-set-faces
 '1am
 '(highlight-indent-face ((t (:background "color-233"))))
 '(font-lock-string-face ((t (:foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "white"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "brightyellow"))))
 '(flymake-errline ((t nil)))
 '(flymake-errline ((t (:foreground "red" :underline "red"))))
 '(flymake-warnline ((t nil)))
 '(flymake-warnline ((t (:foreground "LightBlue2" :underline "LightBlue2"))))
 '(rst-level-1-face ((t (:foreground "cyan"))))
 '(rst-level-2-face ((t (:inherit rst-level-1-face))))
 '(rst-level-3-face ((t (:inherit rst-level-1-face))))
 '(rst-level-4-face ((t (:inherit rst-level-1-face))))
 '(rst-level-5-face ((t (:inherit rst-level-1-face))))
 '(rst-level-6-face ((t (:inherit rst-level-1-face))))
 '(show-paren-match ((t (:background "blue")))))


(provide-theme '1am)
