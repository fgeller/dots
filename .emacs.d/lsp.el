;; (use-package lspce
;;   :ensure (:host github 
;; 		   :repo "zbelial/lspce"
;;            :files (:defaults ,(pcase system-type ('gnu/linux "lspce-module.so") ('darwin "lspce-module.dylib")))
;;            :pre-build ,(pcase system-type
;;                        ('gnu/linux '(("cargo" "build" "--release") ("cp" "./target/release/liblspce_module.so" "./lspce-module.so")))
;;                        ('darwin '(("cargo" "build" "--release") ("cp" "./target/release/liblspce_module.dylib" "./lspce-module.dylib")))))
;;   :commands
;;   (lspce-mode)
;;   :config
;;   (setq lspce-send-changes-idle-time 0.1)
;;   (setq lspce-show-log-level-in-modeline nil)
;;   (lspce-set-log-file "/tmp/lspce.log")
  
;;   (setq lspce-server-programs
;; 		`(("rust"  "rust-analyzer" "")
;; 		  ("python" "jedi-language-server" "" lspce-jedi-initializationOptions)
;; 		  ("python" "pylsp" "" lspce-pylsp-initializationOptions)
;; 		  ("C" "clangd" "")
;; 		  ("java" ,lspce-java-path lspce-jdtls-cmd-args)
;; 		  ("sh" "bash-language-server" "start")
;; 		  ("go" "gopls" "")
;; 		  ("typescript" "typescript-language-server" "--stdio")
;; 		  ("typescriptreact" "typescript-language-server" "--stdio")		
;; 		  ("js" "typescript-language-server" "--stdio")
;; 		  )
;; 		))

; markdown-mode
; f

(use-package flycheck
  :ensure t
  ;; TODO(fg) defer
)

(use-package lsp-mode
  :ensure t
  :commands (lsp-mode)
  :config 
  (setq 
   lsp-ui-sideline-show-hover nil
   lsp-ui-doc-show-with-cursor nil
   lsp-ui-sideline-show-symbol nil
   lsp-ui-sideline-show-code-actions nil
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-enable nil
   lsp-ui-sideline-enable nil
   lsp-modeline-code-actions-enable nil
   lsp-diagnostics-provider :flycheck
   lsp-modeline-diagnostics-scope :file
   lsp-headerline-breadcrumb-enable nil
   lsp-headerline-breadcrumb-icons-enable nil
   lsp-eldoc-render-all nil
   lsp-completion-provider :none
   lsp-lens-enable nil
   lsp-signature-auto-activate nil
   lsp-idle-delay 0.3
   ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
   lsp-enable-symbol-highlighting nil
   lsp-enable-file-watchers nil 
   lsp-file-watch-threshold 5000
   lsp-log-io nil
   lsp-client-packages '(lsp-cmake lsp-dockerfile lsp-go lsp-javascript lsp-json lsp-markdown lsp-terraform lsp-xml lsp-yaml)
   lsp-restart 'ignore)

  (defun fg/lsp-after-open-hook ()
	(setf (alist-get 'styles
					 (alist-get 'lsp-capf completion-category-defaults))
		  '(orderless)))

  (add-hook 'lsp-after-open-hook 'fg/lsp-after-open-hook))


(use-package consult-lsp
  :ensure t
  :commands (consult-lsp-symbols consult-lsp-file-symbols consult-lsp-diagnostics))
