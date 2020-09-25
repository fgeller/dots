(defconst mode-line-wanted-minor-modes '(lsp-mode flycheck-mode))

(setq-default header-line-format
	      `(		
		" " (:propertize "%b" face mode-line-buffer-id)
		(:propertize "%1*" face mode-line)
		(:propertize " %l:%c" face mode-line)
		" " ,(cl-remove-if-not
		      (lambda (p) (member (car p) mode-line-wanted-minor-modes))
		      minor-mode-alist)
		" " mode-line-process
		" "
		))

(setq-default mode-line-format "")

(defun mode-line-bell ()
  (let ((orig-active (face-attribute 'mode-line :background))
	(orig-inactive (face-attribute 'mode-line-inactive :background)))
    (set-face-attribute 'mode-line nil :background "#ff0000")
    (set-face-attribute 'mode-line-highlight nil :background "#ff0000")
    (set-face-attribute 'mode-line-inactive nil :background "#ff0000")
    (sit-for 0.1 t)
    (set-face-attribute 'mode-line nil :background orig-active)
    (set-face-attribute 'mode-line-highlight nil :background orig-active)
    (set-face-attribute 'mode-line-inactive nil :background orig-inactive)))

(setq
 visible-bell nil
 ring-bell-function 'mode-line-bell)
