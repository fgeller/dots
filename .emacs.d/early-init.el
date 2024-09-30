;; https://gist.github.com/axyz/76871b404df376271b521212fba8a621
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
	  load-prefer-newer noninteractive
	  read-process-output-max (* 1 1024 1024)
	  ;package-enable-at-startup nil
	  ffap-machine-p-known 'reject
	  idle-update-delay 1.0
	  frame-inhibit-implied-resize t
	  auto-mode-case-fold nil
	  inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
	  initial-buffer-choice nil
	  inhibit-startup-buffer-menu t
	  bidi-inhibit-bpa t
	  initial-major-mode 'fundamental-mode
	  initial-scratch-message nil
	  inhibit-splash-screen t
	  scroll-bar-mode nil
	  menu-bar-mode nil
	  tool-bar-mode nil
	  use-file-dialog nil
	  use-dialog-box nil
	  init-file-name-handler-alist file-name-handler-alist
	  file-name-handler-alist nil
)

(tooltip-mode -1)
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

;; via https://emacs.stackexchange.com/a/62228/810
;; (let ((font-list (font-family-list)))
;;   (dolist (fn '("Apple Color Emoji" "Noto Color Emoji"))
;; 	(when (member fn font-list)
;; 	  (set-fontset-font t 'symbol fn))))

(set-face-attribute 'default nil :font "Noto Sans Mono-12:weight=book")
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?┃))
(setq default-frame-alist
      (append (list
			   '(left-fringe . 2)
			   '(right-fringe . 2)
			   '(vertical-scroll-bars . nil)
			   '(tool-bar-lines . 0)
			   '(internal-border-width . 8))))
(menu-bar-mode -1)  ;; shows full-screen button for mac port
(global-font-lock-mode -1)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)
