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
		;;mode-line-modes
		mode-line-misc-info
		mode-line-end-spaces))

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

	   
