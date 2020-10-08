(defconst mode-line-wanted-minor-modes '(lsp-mode flycheck-mode))


;; via https://gist.github.com/rougier/4d06d892ded73b4dc6b364a8e0fbcad5
(defun header-line-render (left right)
   (let* ((available-width (- (window-total-width) (length left) )))
     (format (format "%%s%%%ds" available-width) left right)))

(setq-default header-line-format
	      `((:eval
		 (header-line-render
		  (format-mode-line
		   (list
		    (propertize " %b" 'face '(:weight bold))
		    "%1* "
		    (propertize " " 'display '(raise +0.25))
		    (propertize " " 'display '(raise -0.30))
		    ))
		  (format-mode-line
		   (list
		    " " (cl-remove-if-not
			  (lambda (p) (member (car p) mode-line-wanted-minor-modes))
			  minor-mode-alist)
		    " " mode-line-process
		    " %l:%c "))))))

(setq-default mode-line-format "")

(defun mode-line-bell ()
  (let ((orig-active (face-attribute 'mode-line :background))
	(orig-inactive (face-attribute 'mode-line-inactive :background)))
    (set-face-attribute 'mode-line nil :background "#F44336")
    (sit-for 0.1 t)
    (set-face-attribute 'mode-line nil :background orig-active)))

(setq
 visible-bell nil
 ring-bell-function 'mode-line-bell)

(defun modal-mode-visual-toggle ()
  (interactive)
  (let ((faces-to-toggle '(header-line)))
    (cond (modal-mode
           (mapcar (lambda (face)
                     (set-face-background face "#f1f1f1"))
                   faces-to-toggle))
          (t
           (mapcar (lambda (face)
                     (set-face-background face "#FFD54F"))
                   faces-to-toggle)))))

(add-hook 'modal-mode-hook 'modal-mode-visual-toggle)
