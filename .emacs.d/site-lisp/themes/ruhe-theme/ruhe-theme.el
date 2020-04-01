(deftheme ruhe "mostly black on white, some additional material design colors.")

;; https://material.io/design/color/the-color-system.html#tools-for-picking-colors
;;
;; material design 500
;; r ed         #F44336;
;; p ink        #E91E63
;; p urple      #9C27B0
;; deep p urple #673AB7
;; i ndigo      #3F51B5
;; b lue        #2196F3
;; light b lue  #03A9F4
;; c yan        #00BCD4
;; t eal        #009688
;; g reen       #4CAF50
;; light g reen #8BC34A
;; l ime        #CDDC39
;; y ellow      #FFEB3B
;; a mber       #FFC107
;; o range      #FF9800
;; deep o range #FF5722
;; b rown       #795548

;; g ray
;; #FAFAFA
;; #F5F5F5
;; #EEEEEE
;; #E0E0E0
;; #BDBDBD
;; #9E9E9E
;; #757575
;; #616161
;; #424242
;; #212121


;; b lue g ray
;; #ECEFF1
;; #CFD8DC
;; #B0BEC5
;; #90A4AE
;; #78909C
;; #607D8B
;; #546E7A
;; #455A64
;; #37474F
;; #263238

(custom-theme-set-faces
 'ruhe
 `(default ((t (:foreground "#ffffff" :background "#263238"))))
 `(border ((t (:background "#b4b7b4"))))
 `(cursor ((t (:foreground nil :background "#ff5722"))))
 `(fringe ((t (:foreground "#373b41"))))
 `(highlight ((t (:background "#e0e0e0")))) ;ffeb3b
 `(highlight-thing ((t (:background "#37474F" :distant-foreground "#000000"))))
 `(pulse-highlight ((t (:background "#ffeb3b"))))
 `(pulse-highlight-start-face ((t (:background "#ffeb3b"))))
 `(minibuffer-prompt ((t (:foreground "#eceff1" :background "#2196f3"))))
 `(tooltip ((t (:foreground "#eceff1" :background "#78909c"))))

 `(mode-line ((t (:foreground "#eceff1" :background "#4caf50"))))
 `(mode-line-inactive ((t (:foreground "#263238" :background "#cfd8dc"))))
 `(header-line ((t (:foreground "#673ab7" :background nil :inherit mode-line))))
 `(vertical-border ((t (:inherit mode-line))))
 `(region ((t (:background "#455A64" :distant-foreground "#373b41"))))
 `(secondary-selection ((t (:background "#cfd8dc" :distant-foreground "#373b41"))))

 `(trailing-whitespace ((t (:background "#2196f3"))))

 `(widget-button ((t (:underline t))))
 `(widget-field ((t (:background "#b4b7b4" :box (:line-width 1 :color "#282a2e")))))

 `(error ((t (:foreground "#f44336" :weight bold))))
 `(warning ((t (:foreground "#ff5722" :weight bold))))
 `(success ((t (:foreground "#4caf50" :weight bold))))
 `(shadow ((t (:foreground "#b4b7b4"))))

 `(company-tooltip ((t (:inherit tooltip))))
 `(company-scrollbar-bg ((t (:inherit tooltip))))
 `(company-scrollbar-fg ((t (:background "#2196f3"))))
 `(company-tooltip-annotation ((t (:foreground "#cc342b"))))
 `(company-tooltip-common ((t (:foreground "#000000" :inherit highlight-thing))))
 `(company-tooltip-selection ((t (:background "#2196f3"))))
 `(company-tooltip-search ((t (:inherit match))))
 `(company-tooltip-search-selection ((t (:inherit match))))
 `(company-preview-common ((t (:inherit secondary-selection))))
 `(company-preview ((t (:foreground "#969896"))))
 `(company-preview-search ((t (:inherit match))))
 `(company-echo-common ((t (:inherit secondary-selection))))

 `(font-lock-builtin-face ((t (:inherit default))))
 `(font-lock-comment-delimiter-face ((t (:foreground "#90a4ae"))))
 `(font-lock-comment-face ((t (:foreground "#2196f3"))))
 `(font-lock-constant-face ((t (:inherit default))))
 `(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 `(font-lock-doc-string-face ((t (:inherit default))))
 `(font-lock-function-name-face ((t (:inherit default))))
 `(font-lock-keyword-face ((t (:inherit default))))
 `(font-lock-negation-char-face ((t (:inherit default))))
 `(font-lock-preprocessor-face ((t (:inherit default))))
 `(font-lock-regexp-grouping-backslash ((t (:inherit default))))
 `(font-lock-regexp-grouping-construct ((t (:inherit default))))
 `(font-lock-string-face ((t (:inherit default))))
 `(font-lock-type-face ((t (:inherit default))))
 `(font-lock-variable-name-face ((t (:inherit default))))
 `(font-lock-warning-face ((t (:inherit warning))))

 `(git-gutter:added ((t :foreground "#8bc34a" :background "#8bc34a")))
 `(git-gutter:deleted ((t :foreground "#f44336" :background "#f44336")))
 `(git-gutter:modified ((t :foreground "#2196f3" :background "#2196f3")))

 `(ivy-current-match ((t (:foreground "#eceff1" :background "#2196f3"))))
 `(ivy-minibuffer-match-face-1 ((t (:foreground "#a36ac7"))))
 `(ivy-minibuffer-match-face-2 ((t (:foreground "#eceff1" :background "#2196f3"))))
 `(ivy-minibuffer-match-face-3 ((t (:foreground "#eceff1" :background "#2196f3"))))
 `(ivy-minibuffer-match-face-4 ((t (:foreground "#198844"))))
 `(ivy-confirm-face ((t (:foreground "#198844"))))
 `(ivy-match-required-face ((t (:foreground "#cc342b"))))
 `(ivy-virtual ((t (:foreground "#969896"))))
 `(ivy-action ((t (:foreground "#2196f3"))))
 )

(enable-theme 'ruhe)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'ruhe)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; ruhe-theme.el ends here
