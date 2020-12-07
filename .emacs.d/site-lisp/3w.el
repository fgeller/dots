(defconst 3w-minimal-window-width 60
  "Minimal width of a single window. This is used to determined
  if 3w should use a horizontal or vertical split. e.g. if the
  minimal width of a window is 60 and the available width is 170
  columns, then 3w will choose a vertical layout as 170 columns
  cannot support three vertically split windows with a minimal
  width of 60 columns.")

(defconst 3w-side-buffer-store nil
  "Can hold a side-window's buffer")

(defconst 3w-side-window-rx (rx (or "*Backtrace*" "*Compilation*" "*Faces*" "*Help*" "*Messages*" "*Occur*" "*Warnings*" "*ag search"))
  "Regex that matches buffer names that should be displayed in a side window")

(defun 3w-should-split-into-columns-p (count)
  "Indicates whether there is enough space for 3w to split windows into columns"
  (> (frame-total-cols)
     (* count 3w-minimal-window-width)))

(defun 3w-get-side-window-buffer ()
  "Returns buffer displayed in side window if it matches `3w-side-window-rx'"
  (let* ((is-cs (3w-is-column-split-p))
	 (rw (car (window-at-side-list nil 'right)))
	 (bw (car (window-at-side-list nil 'bottom)))
	 (bf (if is-cs (window-buffer rw) (window-buffer bw)))
	 (bn (buffer-name bf)))
    (when (string-match-p 3w-side-window-rx bn)
      bf)))

(defun 3w-split-1 ()
  "Maximizes current window, optionally storing a side window's buffer if present"
  (interactive)
  (let ((buf (3w-get-side-window-buffer)))
    (when buf
      (setq 3w-side-buffer-store buf)
      (message "3w: stored side window buffer %s" buf)))
  (delete-other-windows))

(defun 3w-is-column-split-p ()
  (= (window-size) (1- (frame-total-lines))))

(defun 3w-split-1-1 ()
  "Produces two evenly split windows"
  (interactive)
  (let* ((should-cs (3w-should-split-into-columns-p 2))
	 (is-cs (3w-is-column-split-p))
	 (wc (count-windows)))
    (cond ((= 1 wc)
	   (let* ((nw (if should-cs (split-window-right) (split-window-below)))
		  (ob (or 3w-side-buffer-store (other-buffer))))
	     (when ob (set-window-buffer nw ob))
	     (balance-windows)))

	  ((= 2 wc)
	   (when (xor is-cs should-cs)
	     (let* ((ob (window-buffer (next-window))))
	       (delete-other-windows)
	       (let* ((nw (if should-cs (split-window-right) (split-window-below))))
		 (set-window-buffer nw ob))))
	   (balance-windows))
	  
	  ((= 3 wc)
	   (let* ((sw (car (window-at-side-list nil (if cs 'right 'bottom))))
		  (sb (window-buffer sw)))
	     (when (string-match-p 3w-side-window-rx (buffer-name sb))
	       (setq 3w-side-buffer-store))
	     (delete-window sw)
	     (balance-windows)))
	  
	  (t
	   (delete-other-windows)
	   (3w-split-1-1)) ;; ie wc == 1
	  )))

;; (3w-split-1)
;; (3w-split-1-1)

;; TODO is it possible to listen / add hook to frame resize

(provide '3w)
