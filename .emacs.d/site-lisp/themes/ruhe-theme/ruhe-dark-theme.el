(deftheme ruhe-dark "mostly white on dark, some additional material design colors.")

;; https://material.io/design/color/the-color-system.html#tools-for-picking-colors
;;
;; material design 500
;; r ed         #F44336
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
 'ruhe-dark
 `(default ((t (:foreground "#f5f5f5" :background "#282828"))))
 `(fixed-pitch ((t (:inherit default))))
 `(underline ((t (:inherit default :underline unspecified))))
 `(border ((t (:background "#b4b7b4"))))
 `(cursor ((t (:foreground "#ffffff" :background "#F44336"))))
 `(fringe ((t (:inherit default))))
 `(internal-border ((t (:inherit default))))
 `(highlight ((t (:background "#8e5ff4" :distant-foreground "#FFEB3B"))))
 `(hi-yellow ((t (:inherit highlight))))
 `(match ((t (:inherit highlight))))
 `(pulse-highlight ((t (:background "#DCEDC8"))))
 ;; `(highlight-thing ((t (:background "#ffd54f" :distant-foreground "#000000"))))
 `(pulse-highlight-start-face ((t (:background "#ffd54f"))))
 `(minibuffer-prompt ((t (:foreground "#78909c" :background unspecified))))
 `(tooltip ((t (:foreground "#eceff1" :background "#78909c"))))
 
 ;;`(mode-line ((t (:background "#ffffff" :foreground "#ffffff"))))
 `(mode-line ((t (:background "#1565c0" :inherit default))))
 `(mode-line-highlight ((t (:inherit mode-line))))
 `(mode-line-buffer-id ((t (:inherit mode-line-highlight))))
 `(mode-line-emphasis ((t (:inherit mode-line-highlight))))
 `(header-line ((t (:background "#757575" :foreground "#f5f5f5" :inherit unspecified))))
 `(mode-line-inactive ((t (:inherit mode-line))))
 `(vertical-border ((t (:background "#ff0000" :foreground "#cfd8dc" :underline unspecified))))

 `(region ((t (:background "#3F51B5"))))
 `(secondary-selection ((t (:background "#cfd8dc" :distant-foreground "#373b41"))))

 `(trailing-whitespace ((t (:background "#2196f3"))))
 `(show-paren-match ((t (:background "#43A047" :foreground "#ffffff"))))

 `(button ((t (:foreground "#03A9F4" :underline unspecified))))
 `(widget-button ((t (:foreground "#03A9F4" :underline unspecified))))
 `(widget-field ((t (:background "#b4b7b4" :box (:line-width 1 :color "#282a2e")))))
 `(browse-url-button ((t (:foreground "#03A9F4" :underline unspecified))))
 `(info-menu-header ((t (:weight normal :underline unspecified))))
 `(info-xref ((t (:foreground "#03A9F4" :underline unspecified))))
 `(info-xref-visited ((t (:foreground "#9C27B0" :underline unspecified))))
 `(link ((t (:foreground "#03A9F4" :underline unspecified))))
 `(link-visited ((t (:foreground "#9C27B0" :underline unspecified))))

 `(error ((t (:background "#f44336" :foreground "#ffffff"))))
 `(warning ((t (:foreground "#ff5722" :weight bold))))
 `(success ((t (:foreground "#4caf50" :weight bold :underline unspecified))))
 `(shadow ((t (:foreground "#b4b7b4"))))

 `(completions-annotations ((t (:foreground "#4caf50"))))
 `(completions-common-part ((t (:background "#590de5" :foreground "red"))))
 `(completions-first-difference ((t (:background unspecified :foreground unspecified))))

 `(compilation-error ((t (:foreground "#f44336" :background unspecified))))
 `(compilation-info ((t (:foreground "#ff5722" :background unspecified))))
 `(compilation-mode-line-exit ((t (:foreground "#000000" :background unspecified))))
 `(compilation-mode-line-fail ((t (:foreground "#f44336" :background unspecified))))
 `(compilation-mode-line-run ((t (:inherit success))))
 `(compilation-warning ((t (:inherit warning))))

 `(consult-file ((t (:inherit unspecified))))
 `(embark-collect-group-title ((t (:foreground "#aaaaaa" :inherit unspecified))))

 `(eshell-prompt ((t (:foreground "#aaaaaa" :background unspecified))))

 `(lsp-headerline-breadcrumb-path-face ((t (:inherit unspecified))))
 `(lsp-headerline-breadcrumb-path-error-face ((t (:inherit error :underline unspecified))))
 `(lsp-headerline-breadcrumb-path-warning-face ((t (:inherit warning :underline unspecified))))
 `(lsp-headerline-breadcrumb-path-hint-face ((t (:underline unspecified))))
 `(lsp-headerline-breadcrumb-path-info-face ((t (:underline unspecified))))
 `(lsp-headerline-breadcrumb-symbols-info-face ((t (:underline unspecified))))
 `(lsp-headerline-breadcrumb-symbols-hint-face ((t (:underline unspecified))))
 `(lsp-headerline-breadcrumb-symbols-face ((t (:inherit unspecified))))
 `(lsp-flycheck-error-unnecessary-face ((t (:foreground "#ff5722"))))
 
 `(icomplete-first-match ((t (:foreground "#000000" :background "#FFD54F"))))

 `(avy-background-face ((t (:foreground "#aaaaaa"))))
 `(avy-goto-char-timer-face ((t (:foreground "#efefef" :background "#FFD54F"))))
 `(avy-lead-face ((t (:foreground "#ff5722" :weight bold))))
 `(avy-lead-face-0 ((t (:foreground "#1565c0"))))
 `(avy-lead-face-1 ((t (:foreground "#1565c0"))))
 `(avy-lead-face-2 ((t (:foreground "#1565c0"))))

 ;; test comment
 `(font-lock-builtin-face ((t (:foreground "#f5f5f5" ))))
 `(font-lock-comment-delimiter-face ((t (:foreground "#C5CAE9" :background "#424242"))))
 `(font-lock-comment-face ((t (:foreground unspecified :background "#424242" :underline unspecified))))
 `(font-lock-doc-face ((t (:foreground unspecified :background "#424242" :underline unspecified))))
 `(font-lock-constant-face ((t (:foreground "#f5f5f5"))))
 `(font-lock-doc-string-face ((t (:foreground "#f5f5f5"))))
 `(font-lock-function-name-face ((t (:foreground "#f5f5f5" ))))
 `(font-lock-keyword-face ((t (:foreground "#f5f5f5" ))))
 `(font-lock-negation-char-face ((t (:foreground "#f5f5f5" ))))
 `(font-lock-preprocessor-face ((t (:foreground "#f5f5f5" ))))
 `(font-lock-regexp-grouping-backslash ((t (:foreground "#f5f5f5" ))))
 `(font-lock-regexp-grouping-construct ((t (:foreground "#f5f5f5" ))))
 `(font-lock-string-face ((t (:foreground "#f5f5f5" ))))
 `(font-lock-type-face ((t (:foreground "#f5f5f5" ))))
 `(font-lock-variable-name-face ((t (:foreground "#f5f5f5" ))))
 `(font-lock-warning-face ((t (:inherit warning ))))

 `(diff-function ((t (:inherit default ))))
 `(diff-header ((t (:inherit default ))))
 `(diff-index ((t (:inherit default ))))
 `(diff-file-header ((t (:inherit default ))))
 `(diff-refine-added ((t (:foreground "#4CAF50" ))))
 `(diff-refine-removed ((t (:foreground "#f44336" ))))
 `(diff-refine-changed ((t (:foreground "#FFEB3B" ))))

 `(ediff-fine-diff-A ((t (:foreground "#f44336" ))))
 `(ediff-fine-diff-B ((t (:foreground "#4CAF50" ))))
 
 `(linum ((t (:inherit default))))

 `(flycheck-error ((t (:foreground "#f44336" :background unspecified))))
 `(flycheck-error-list-id ((t (:foreground "#f44336" :background unspecified))))
 `(flycheck-warning ((t (:inherit warning))))

 `(diff-hl-margin-insert ((t (:background "#4caf50"))))
 `(diff-hl-margin-delete ((t (:background "#f44336"))))
 `(diff-hl-margin-change ((t (:background "#1565c0"))))
 )

(enable-theme 'ruhe-dark)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'ruhe-dark)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; ruhe-dark-theme.el ends here
