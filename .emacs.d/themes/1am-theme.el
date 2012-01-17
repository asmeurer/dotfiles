(deftheme 1am
  "Theme meant for green text on semi-transparent black
  background. Loosely based on XCode's midnight theme (hence the name).")

(custom-theme-set-faces
 'Midnight
 '(highlight-indent-face ((t (:background "color-233"))))
 '(font-lock-string-face ((t (:foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "white"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "brightyellow")))))

(provide-theme 'Midnight)
