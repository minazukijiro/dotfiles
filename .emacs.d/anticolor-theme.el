(deftheme anticolor
  "anticolor theme")

(custom-theme-set-faces
 'anticolor

 '(header-line ((t (:inherit nil :inverse-video nil :underline "black"))))
 '(region ((t (:background "yellow" :foreground "black"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(font-lock-builtin-face ((t (:weight ultra-bold))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-constant-face ((t (:slant italic :weight semi-bold))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((t (:slant oblique :weight semi-bold))))
 '(font-lock-keyword-face ((t (:weight extra-bold))))
 '(font-lock-preprocessor-face ((t (:slant oblique))))
 '(font-lock-string-face ((t (:underline "yellow"))))
 '(font-lock-type-face ((t (:slant italic))))
 '(font-lock-variable-name-face ((t (:slant italic))))
 '(font-lock-warning-face ((t (:underline (:color "red" :style wave) :weight ultra-bold))))
 '(link ((t (:underline "green" :slant italic)))))

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'anticolor)
