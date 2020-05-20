
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
 `(default ((t (:foreground "#000000" :background "#ffffff"))))
 `(border ((t (:background "#b4b7b4"))))
 `(cursor ((t (:foreground "#ffffff" :background "#ff5722"))))
 `(fringe ((t (:foreground "#373b41"))))
 `(highlight ((t (:background "#DCEDC8" :distant-foreground "#000000"))))
 `(pulse-highlight ((t (:background "#DCEDC8"))))
 `(highlight-thing ((t (:background "#FFF9C4" :distant-foreground "#000000"))))
 `(pulse-highlight-start-face ((t (:background "#ffeb3b"))))
 `(minibuffer-prompt ((t (:foreground "#eceff1" :background "#2196f3"))))
 `(tooltip ((t (:foreground "#eceff1" :background "#78909c"))))

 `(mode-line ((t (:foreground "#eceff1" :background "#4caf50"))))
 `(header-line ((t (:inherit mode-line))))
 `(mode-line-inactive ((t (:foreground "#263238" :background "#cfd8dc"))))
 `(vertical-border ((t (:inherit mode-line))))
 `(region ((t (:background "#BBDEFB"))))
 `(secondary-selection ((t (:background "#cfd8dc" :distant-foreground "#373b41"))))

 `(trailing-whitespace ((t (:background "#2196f3"))))
 `(show-paren-match ((t (:inherit cursor))))

 `(widget-button ((t (:underline t))))
 `(widget-field ((t (:background "#b4b7b4" :box (:line-width 1 :color "#282a2e")))))

 `(error ((t (:foreground "#f44336" :weight bold))))
 `(warning ((t (:foreground "#ff5722" :weight bold))))
 `(success ((t (:foreground "#4caf50" :weight bold))))
 `(shadow ((t (:foreground "#b4b7b4"))))

 `(completions-annotations ((t (:foreground "#3F51B5"))))
 `(completions-common-part ((t (:background "#2196f3" :foreground "#ffffff"))))
 `(completions-first-difference ((t (:background nil :foreground nil))))
 
 `(live-completions-forceable-candidate ((t (:foreground "#000000" :background "#DCEDC8"))))

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
 `(font-lock-comment-delimiter-face ((t (:foreground "#C5CAE9"))))
 `(font-lock-comment-face ((t (:foreground "#3F51B5"))))
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

 `(git-gutter:added ((t :foreground "#DCEDC8" :background "#DCEDC8")))
 `(git-gutter:deleted ((t :foreground "#EF9A9A" :background "#EF9A9A")))
 `(git-gutter:modified ((t :foreground "#90CAF9" :background "#90CAF9")))

 `(ivy-current-match ((t (:foreground "#eceff1" :background "#2196f3"))))
 `(ivy-minibuffer-match-face-1 ((t (:foreground "#a36ac7"))))
 `(ivy-minibuffer-match-face-2 ((t (:foreground "#eceff1" :background "#2196f3"))))
 `(ivy-minibuffer-match-face-3 ((t (:foreground "#eceff1" :background "#2196f3"))))
 `(ivy-minibuffer-match-face-4 ((t (:foreground "#198844"))))
 `(ivy-confirm-face ((t (:foreground "#198844"))))
 `(ivy-match-required-face ((t (:foreground "#cc342b"))))
 `(ivy-virtual ((t (:foreground "#969896"))))
 `(ivy-action ((t (:foreground "#2196f3"))))
 
 ;; `(magit-bisect-bad ((t (:background nil :foreground nil))))
 ;; `(magit-bisect-good ((t (:background nil :foreground nil))))
 ;; `(magit-bisect-skip ((t (:background nil :foreground nil))))
 ;; `(magit-blame-date ((t (:background nil :foreground nil))))
 ;; `(magit-blame-dimmed ((t (:background nil :foreground nil))))
 ;; `(magit-blame-hash ((t (:background nil :foreground nil))))
 ;; `(magit-blame-heading ((t (:background nil :foreground nil))))
 ;; `(magit-blame-highlight ((t (:background nil :foreground nil))))
 ;; `(magit-blame-margin ((t (:background nil :foreground nil))))
 ;; `(magit-blame-name ((t (:background nil :foreground nil))))
 ;; `(magit-blame-summary ((t (:background nil :foreground nil))))
 ;; `(magit-branch-current ((t (:background nil :foreground nil))))
 ;; `(magit-branch-local ((t (:background nil :foreground nil))))
 ;; `(magit-branch-remote ((t (:background nil :foreground nil))))
 ;; `(magit-branch-remote-head ((t (:background nil :foreground nil))))
 ;; `(magit-branch-upstream ((t (:background nil :foreground nil))))
 ;; `(magit-cherry-equivalent ((t (:background nil :foreground nil))))
 ;; `(magit-cherry-unmatched ((t (:background nil :foreground nil))))
 `(magit-diff-added ((t (:background "#F1F8E9"))))
 `(magit-diff-added-highlight ((t (:background "#E8F5E9"))))
 ;; `(magit-diff-base ((t (:background nil :foreground nil))))
 ;; `(magit-diff-base-highlight ((t (:background nil :foreground nil))))
 ;; `(magit-diff-conflict-heading ((t (:background nil :foreground nil))))
 `(magit-diff-context ((t (:background nil :foreground "#b0bec5"))))
 `(magit-diff-context-highlight ((t (:background nil))))
 ;; `(magit-diff-file-heading ((t (:background nil :foreground nil))))
 ;; `(magit-diff-file-heading-highlight ((t (:background nil :foreground nil))))
 ;; `(magit-diff-file-heading-selection ((t (:background nil :foreground nil))))
 ;; `(magit-diff-hunk-heading ((t (:background nil :foreground nil))))
 ;; `(magit-diff-hunk-heading-highlight ((t (:background nil :foreground nil))))
 ;; `(magit-diff-hunk-heading-selection ((t (:background nil :foreground nil))))
 ;; `(magit-diff-hunk-region ((t (:background nil :foreground nil))))
 ;; `(magit-diff-lines-boundary ((t (:background nil :foreground nil))))
 ;; `(magit-diff-lines-heading ((t (:background nil :foreground nil))))
 ;; `(magit-diff-our ((t (:background nil :foreground nil))))
 ;; `(magit-diff-our-highlight ((t (:background nil :foreground nil))))
 `(magit-diff-removed ((t (:background "#FBE9E7"))))
 `(magit-diff-removed-highlight ((t (:background "#FFCCBC"))))
 ;; `(magit-diff-revision-summary ((t (:background nil :foreground nil))))
 ;; `(magit-diff-revision-summary-highlight ((t (:background nil :foreground nil))))
 ;; `(magit-diff-their ((t (:background nil :foreground nil))))
 ;; `(magit-diff-their-highlight ((t (:background nil :foreground nil))))
 ;; `(magit-diff-whitespace-warning ((t (:background nil :foreground nil))))
 ;; `(magit-diffstat-added ((t (:background nil :foreground nil))))
 ;; `(magit-diffstat-removed ((t (:background nil :foreground nil))))
 ;; `(magit-dimmed ((t (:background nil :foreground nil))))
 ;; `(magit-filename ((t (:background nil :foreground nil))))
 ;; `(magit-hash ((t (:background nil :foreground nil))))
 ;; `(magit-head ((t (:background nil :foreground nil))))
 ;; `(magit-header-line ((t (:background nil :foreground nil))))
 ;; `(magit-header-line-key ((t (:background nil :foreground nil))))
 ;; `(magit-header-line-log-select ((t (:background nil :foreground nil))))
 ;; `(magit-keyword ((t (:background nil :foreground nil))))
 ;; `(magit-keyword-squash ((t (:background nil :foreground nil))))
 ;; `(magit-log-author ((t (:background nil :foreground nil))))
 ;; `(magit-log-date ((t (:background nil :foreground nil))))
 ;; `(magit-log-graph ((t (:background nil :foreground nil))))
 ;; `(magit-mode-line-process ((t (:background nil :foreground nil))))
 ;; `(magit-mode-line-process-error ((t (:background nil :foreground nil))))
 ;; `(magit-process-ng ((t (:background nil :foreground nil))))
 ;; `(magit-process-ok ((t (:background nil :foreground nil))))
 ;; `(magit-reflog-amend ((t (:background nil :foreground nil))))
 ;; `(magit-reflog-checkout ((t (:background nil :foreground nil))))
 ;; `(magit-reflog-cherry-pick ((t (:background nil :foreground nil))))
 ;; `(magit-reflog-commit ((t (:background nil :foreground nil))))
 ;; `(magit-reflog-merge ((t (:background nil :foreground nil))))
 ;; `(magit-reflog-other ((t (:background nil :foreground nil))))
 ;; `(magit-reflog-rebase ((t (:background nil :foreground nil))))
 ;; `(magit-reflog-remote ((t (:background nil :foreground nil))))
 ;; `(magit-reflog-reset ((t (:background nil :foreground nil))))
 ;; `(magit-refname ((t (:background nil :foreground nil))))
 ;; `(magit-refname-pullreq ((t (:background nil :foreground nil))))
 ;; `(magit-refname-stash ((t (:background nil :foreground nil))))
 ;; `(magit-refname-wip ((t (:background nil :foreground nil))))
 ;; `(magit-section-heading ((t (:background nil :foreground nil))))
 ;; `(magit-section-heading-selection ((t (:background nil :foreground nil))))
 ;; `(magit-section-highlight ((t (:background nil :foreground nil))))
 ;; `(magit-section-secondary-heading ((t (:background nil :foreground nil))))
 ;; `(magit-sequence-done ((t (:background nil :foreground nil))))
 ;; `(magit-sequence-drop ((t (:background nil :foreground nil))))
 ;; `(magit-sequence-exec ((t (:background nil :foreground nil))))
 ;; `(magit-sequence-head ((t (:background nil :foreground nil))))
 ;; `(magit-sequence-onto ((t (:background nil :foreground nil))))
 ;; `(magit-sequence-part ((t (:background nil :foreground nil))))
 ;; `(magit-sequence-pick ((t (:background nil :foreground nil))))
 ;; `(magit-sequence-stop ((t (:background nil :foreground nil))))
 ;; `(magit-signature-bad ((t (:background nil :foreground nil))))
 ;; `(magit-signature-error ((t (:background nil :foreground nil))))
 ;; `(magit-signature-expired ((t (:background nil :foreground nil))))
 ;; `(magit-signature-expired-key ((t (:background nil :foreground nil))))
 ;; `(magit-signature-good ((t (:background nil :foreground nil))))
 ;; `(magit-signature-revoked ((t (:background nil :foreground nil))))
 ;; `(magit-signature-untrusted ((t (:background nil :foreground nil))))
 ;; `(magit-tag ((t (:background nil :foreground nil))))

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
