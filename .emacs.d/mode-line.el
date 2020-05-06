(defconst mode-line-wanted-minor-modes '(lsp-mode flycheck-mode))

(defun mode-line-filler (left right)
  (let ((len (- (window-total-width) (length left) (length right))))
    (format (format "%%%ss" len) " ")))

(setq header-line-format-left `(" "))

(setq header-line-format-right
      `(
	" " "%e"
	" " ,(cl-remove-if-not
	      (lambda (p) (member (car p) mode-line-wanted-minor-modes))
	      minor-mode-alist)
	" " mode-line-process
	))

(setq mode-line-format-left `(" " (:propertize "%b" face mode-line-buffer-id) " " "%1*"))

(setq mode-line-format-right `(" " "l" "%l" " " "c" "%c" " "))

(defun make-mode-line-format ()
  (let* ((left (format-mode-line mode-line-format-left))
	 (right (format-mode-line mode-line-format-right))
	 (filler (mode-line-filler left right)))
    (concat left filler right)))

(defun make-header-line-format ()
  (let* ((left (format-mode-line header-line-format-left))
	 (right (format-mode-line header-line-format-right))
	 (filler (mode-line-filler left right)))
    (concat left filler right)))

(setq-default mode-line-format
	      `(
		" " (:propertize "%b" face mode-line-buffer-id)
		" " "%1*"
		" " "%l" "/" "%c"
		" "
		))
(setq-default header-line-format
	      `(
		" " ,(cl-remove-if-not
		      (lambda (p) (member (car p) mode-line-wanted-minor-modes))
		      minor-mode-alist)
		" " mode-line-process
		""
		))

(defun mode-line-bell ()
  (let ((orig-active (face-attribute 'mode-line :background))
	(orig-inactive (face-attribute 'mode-line-inactive :background)))
    (set-face-attribute 'mode-line nil :background "red")
    (set-face-attribute 'mode-line-inactive nil :background "red")
    (sit-for 0.1 t)
    (set-face-attribute 'mode-line nil :background orig-active)
    (set-face-attribute 'mode-line-inactive nil :background orig-inactive)))

(setq
 visible-bell nil
 ring-bell-function 'mode-line-bell)
