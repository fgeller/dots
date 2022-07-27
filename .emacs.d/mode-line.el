;; -*- lexical-binding: t; -*-

(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
		mode-line-mule-info
		mode-line-client
		mode-line-modified
		mode-line-remote
		mode-line-frame-identification
		mode-line-buffer-identification
		"   "
		mode-line-position
		evil-mode-line-tag
		(vc-mode vc-mode)
		"  "
		mode-line-modes
		mode-line-misc-info
		mode-line-end-spaces))

(install 'diminish)

(after 'company (diminish 'company-mode))
(after 'modal (diminish 'modal-mode))
(after 'highlight-thing (diminish 'highlight-thing-mode))
(after 'yasnippet (diminish 'yas-minor-mode))
(after 'eldoc (diminish 'eldoc-mode))
(after 'tree-sitter (diminish 'tree-sitter-mode))
(after 'subword (diminish 'subword-mode))
(after 'hi-lock (diminish 'hi-lock-mode))

(defun modal-mode-visual-toggle ()
  (interactive)
  (let ((faces-to-toggle '(mode-line)))
    (cond (modal-mode
	   (mapcar (lambda (face)
		     (set-face-background face "#1565c0")
		     (set-face-foreground face "white"))
		   faces-to-toggle))
	  (t
	   (mapcar (lambda (face)
		     (set-face-background face "#ffd54f")
		     (set-face-foreground face "black"))
		   faces-to-toggle)))))
(add-hook 'modal-mode-hook 'modal-mode-visual-toggle)

(defun mode-line-bell ()
  (let ((orig-active (face-attribute 'mode-line :background)))
    (set-face-attribute 'mode-line nil :background "#f44336")
    (sit-for 0.1 t)
    (set-face-attribute 'mode-line nil :background orig-active)))

(setq
 visible-bell nil
 ring-bell-function 'mode-line-bell)

	   
