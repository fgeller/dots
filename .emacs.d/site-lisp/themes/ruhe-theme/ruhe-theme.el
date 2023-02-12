(deftheme ruhe "mostly black on white, some additional material design colors.")

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
 'ruhe
 `(default ((t (:foreground "#000000"))))
 `(fixed-pitch ((t (:inherit default))))
 `(underline ((t (:inherit default :underline unspecified))))
 `(border ((t (:background "#b4b7b4"))))
 `(cursor ((t (:foreground "#ffffff" :background "#F44336"))))
 `(fringe ((t (:foreground "#000000" :background "#ffffff"))))
 `(internal-border ((t (:background "#ffffff"))))
 `(highlight ((t (:background "#ffeeb5" :distant-foreground "#000000"))))
 `(hi-yellow ((t (:inherit highlight))))
 `(match ((t (:inherit highlight))))
 `(pulse-highlight ((t (:background "#DCEDC8"))))
 ;; `(highlight-thing ((t (:background "#ffd54f" :distant-foreground "#000000"))))
 `(pulse-highlight-start-face ((t (:background "#ffd54f"))))
 `(minibuffer-prompt ((t (:foreground "#78909c" :background "#ffffff"))))
 `(tooltip ((t (:foreground "#eceff1" :background "#78909c"))))
 
 ;;`(mode-line ((t (:background "#ffffff" :foreground "#ffffff"))))
 `(mode-line ((t (:background "#1565c0" :foreground "#ffffff"))))
 `(mode-line-highlight ((t (:inherit mode-line))))
 `(mode-line-buffer-id ((t (:inherit mode-line-highlight))))
 `(mode-line-emphasis ((t (:inherit mode-line-highlight))))
 `(header-line ((t (:background "#f1f1f1" :foreground "#000000"))))
 `(mode-line-inactive ((t (:inherit mode-line))))
 `(vertical-border ((t (:background "#ffffff" :foreground "#cfd8dc" :underline unspecified))))

 `(region ((t (:background "#BBDEFB"))))
 `(secondary-selection ((t (:background "#cfd8dc" :distant-foreground "#373b41"))))

 `(trailing-whitespace ((t (:background "#2196f3"))))
 `(show-paren-match ((t (:background "#43A047" :foreground "#ffffff"))))

 `(button ((t (:foreground "#3F51B5" :underline unspecified))))
 `(widget-button ((t (:foreground "#3F51B5" :underline unspecified))))
 `(widget-field ((t (:background "#b4b7b4" :box (:line-width 1 :color "#282a2e")))))
 `(browse-url-button ((t (:foreground "#3F51B5" :underline unspecified))))
 `(info-menu-header ((t (:weight normal :underline unspecified))))
 `(info-xref ((t (:foreground "#3F51B5" :underline unspecified))))
 `(info-xref-visited ((t (:foreground "#9C27B0" :underline unspecified))))
 `(link ((t (:foreground "#3F51B5" :underline unspecified))))
 `(link-visited ((t (:foreground "#9C27B0" :underline unspecified))))

 `(error ((t (:background "#f44336" :foreground "#ffffff"))))
 `(warning ((t (:foreground "#ff5722" :weight bold))))
 `(success ((t (:foreground "#4caf50" :weight bold :underline unspecified))))
 `(shadow ((t (:foreground "#b4b7b4"))))

 `(completions-annotations ((t (:foreground "#3F51B5"))))
 `(completions-common-part ((t (:background "#2196f3" :foreground "#ffffff"))))
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
 `(lsp-headerline-breadcrumb-symbols-face ((t (:inherit unspecified))))
 `(lsp-lsp-flycheck-error-unnecessary-face ((t (:foreground "#ff5722"))))
 
 `(icomplete-first-match ((t (:foreground "#000000" :background "#FFD54F"))))

 `(avy-background-face ((t (:foreground "#aaaaaa"))))
 `(avy-goto-char-timer-face ((t (:foreground "#efefef" :background "#FFD54F"))))
 `(avy-lead-face ((t (:foreground "#ff5722" :weight bold))))
 `(avy-lead-face-0 ((t (:foreground "#1565c0"))))
 `(avy-lead-face-1 ((t (:foreground "#1565c0"))))
 `(avy-lead-face-2 ((t (:foreground "#1565c0"))))

 `(font-lock-builtin-face ((t (:foreground "#000000" ))))
 `(font-lock-comment-delimiter-face ((t (:foreground "#C5CAE9" :background "#f1f1f1"))))
 `(font-lock-comment-face ((t (:foreground unspecified :background "#f1f1f1" :underline unspecified))))
 `(font-lock-doc-face ((t (:foreground unspecified :background "#f1f1f1" :underline unspecified))))
 `(font-lock-constant-face ((t (:foreground "#000000" ))))
 `(font-lock-doc-string-face ((t (:foreground "#000000" ))))
 `(font-lock-function-name-face ((t (:foreground "#000000" ))))
 `(font-lock-keyword-face ((t (:foreground "#000000" ))))
 `(font-lock-negation-char-face ((t (:foreground "#000000" ))))
 `(font-lock-preprocessor-face ((t (:foreground "#000000" ))))
 `(font-lock-regexp-grouping-backslash ((t (:foreground "#000000" ))))
 `(font-lock-regexp-grouping-construct ((t (:foreground "#000000" ))))
 `(font-lock-string-face ((t (:foreground "#000000" ))))
 `(font-lock-type-face ((t (:foreground "#000000" ))))
 `(font-lock-variable-name-face ((t (:foreground "#000000" ))))
 `(font-lock-warning-face ((t (:inherit warning ))))

 `(linum ((t (:foreground "#aaaaaa" :background "#f5f5f5"))))

 `(flycheck-error ((t (:foreground "#f44336" :background unspecified))))
 `(flycheck-error-list-id ((t (:foreground "#f44336" :background unspecified))))
 `(flycheck-warning ((t (:inherit warning))))

 `(diff-hl-margin-insert ((t (:background "#4caf50"))))
 `(diff-hl-margin-delete ((t (:background "#f44336"))))
 `(diff-hl-margin-change ((t (:background "#1565c0"))))

 ;; `(magit-bisect-bad ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-bisect-good ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-bisect-skip ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-blame-date ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-blame-dimmed ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-blame-hash ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-blame-heading ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-blame-highlight ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-blame-margin ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-blame-name ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-blame-summary ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-branch-current ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-branch-local ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-branch-remote ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-branch-remote-head ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-branch-upstream ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-cherry-equivalent ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-cherry-unmatched ((t (:background unspecified :foreground unspecified))))
 `(magit-diff-added ((t (:background "#E8F5E9"))))
 `(magit-diff-added-highlight ((t (:background "#E8F5E9"))))
 `(magit-diff-base ((t (:background unspecified :foreground unspecified))))
 `(magit-diff-base-highlight ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-diff-conflict-heading ((t (:background unspecified :foreground unspecified))))
 `(magit-diff-context ((t (:background unspecified :foreground unspecified))))
 `(magit-diff-context-highlight ((t (:background unspecified))))
 ;; `(magit-diff-file-heading ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-diff-file-heading-highlight ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-diff-file-heading-selection ((t (:background unspecified :foreground unspecified))))
 `(magit-diff-hunk-heading ((t (:background "#f1f1f1" :foreground unspecified))))
 `(magit-diff-hunk-heading-highlight ((t (:background "#FFEB3B" :foreground unspecified))))
 `(magit-diff-hunk-heading-selection ((t (:background unspecified :foreground unspecified))))
 `(magit-diff-hunk-region ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-diff-lines-boundary ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-diff-lines-heading ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-diff-our ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-diff-our-highlight ((t (:background unspecified :foreground unspecified))))
 `(magit-diff-removed ((t (:background "#FFCCBC"))))
 `(magit-diff-removed-highlight ((t (:background "#FFCCBC"))))
 ;; `(magit-diff-revision-summary ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-diff-revision-summary-highlight ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-diff-their ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-diff-their-highlight ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-diff-whitespace-warning ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-diffstat-added ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-diffstat-removed ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-dimmed ((t (:background unspecified :foreground unspecified))))
 `(magit-filename ((t (:background unspecified :foreground unspecified :weight normal))))
 ;; `(magit-hash ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-head ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-header-line ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-header-line-key ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-header-line-log-select ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-keyword ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-keyword-squash ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-log-author ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-log-date ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-log-graph ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-mode-line-process ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-mode-line-process-error ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-process-ng ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-process-ok ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-reflog-amend ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-reflog-checkout ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-reflog-cherry-pick ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-reflog-commit ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-reflog-merge ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-reflog-other ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-reflog-rebase ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-reflog-remote ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-reflog-reset ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-refname ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-refname-pullreq ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-refname-stash ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-refname-wip ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-section-heading ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-section-heading-selection ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-section-highlight ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-section-secondary-heading ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-sequence-done ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-sequence-drop ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-sequence-exec ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-sequence-head ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-sequence-onto ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-sequence-part ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-sequence-pick ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-sequence-stop ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-signature-bad ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-signature-error ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-signature-expired ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-signature-expired-key ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-signature-good ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-signature-revoked ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-signature-untrusted ((t (:background unspecified :foreground unspecified))))
 ;; `(magit-tag ((t (:background unspecified :foreground unspecified))))

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
