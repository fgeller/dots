(install 'lsp-mode)
(install 'lsp-ui)
(install 'lsp-origami)

(setq
 lsp-ui-sideline-show-hover nil
 lsp-ui-doc-show-with-cursor nil
 lsp-ui-sideline-show-symbol nil
 lsp-ui-sideline-show-code-actions nil
 lsp-ui-doc-position 'at-point
 lsp-ui-doc-enable nil
 lsp-ui-sideline-enable t
 lsp-modeline-diagnostics-scope :file
 lsp-headerline-breadcrumb-enable t
 lsp-eldoc-render-all nil
 lsp-completion-provider :none
 lsp-idle-delay 1.0
 ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
 lsp-enable-symbol-highlighting nil
 lsp-enable-file-watchers t 
 lsp-file-watch-threshold 1000
 lsp-log-io nil
 lsp-client-packages '(lsp-cmake lsp-dockerfile lsp-go lsp-javascript lsp-json lsp-markdown lsp-terraform lsp-xml lsp-yaml)
)

(add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)

;; TODO disable unused ones?
;;   (dolist (feature '(lsp-ui-peek lsp-ui-sideline lsp-ui-doc lsp-ui-imenu))
