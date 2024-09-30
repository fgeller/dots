;; -*- lexical-binding: t; -*-

(size-indication-mode -1)
(setq mode-line-end-spaces '(:eval (unless (display-graphic-p) " ")))
(setq-default mode-line-format
			  `("%e"
				;mode-line-front-space
				;mode-line-mule-info
				;mode-line-client
				mode-line-modified
				;mode-line-remote
				,(list (propertize " " 'face 'mode-line-buffer-id))
				mode-line-buffer-identification
				,(list (propertize " " 'face 'mode-line-buffer-id))
				
				"   "
				mode-line-position
				" "
				mode-line-modes
				mode-line-misc-info
				,(propertize " " 'display '(raise +0.25))
				" "
				,(propertize " " 'display '(raise -0.25))
				mode-line-end-spaces))

(use-package diminish
  :ensure t
  :commands diminish
  :init
  (after 'company (diminish 'company-mode))
  (after 'modal (diminish 'modal-mode))
  (after 'highlight-thing (diminish 'highlight-thing-mode))
  (after 'yasnippet (diminish 'yas-minor-mode))
  (after 'eldoc (diminish 'eldoc-mode))
  (after 'tree-sitter (diminish 'tree-sitter-mode))
  (after 'subword (diminish 'subword-mode))
  (after 'hi-lock (diminish 'hi-lock-mode))
  (after 'apheleia (diminish 'apheleia-mode))
)

(defun modal-mode-visual-toggle ()
  (interactive)
  (let ((faces-to-toggle '(mode-line-buffer-id)))
    (cond (modal-mode
	   (mapcar (lambda (face)
		     (set-face-background face "#ffd54f")
		     (set-face-foreground face "#000000"))
		   faces-to-toggle))
	  (t
	   (mapcar (lambda (face)
		     (set-face-background face "#8BC34A")
		     (set-face-foreground face "#000000"))
		   faces-to-toggle)))))
(add-hook 'modal-mode-hook 'modal-mode-visual-toggle)

(defun mode-line-bell ()
  (let ((orig-active (face-attribute 'mode-line-buffer-id :background)))
    (set-face-attribute 'mode-line-buffer-id nil :background "#f44336")
    (sit-for 0.1 t)
    (set-face-attribute 'mode-line-buffer-id nil :background orig-active)))

(setq
 visible-bell nil
 ring-bell-function 'mode-line-bell)
