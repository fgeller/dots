;; TODO read up on quit-restore window-parameter, cf quit-restore-window

(defconst cv-side-window-rx (rx (or "*Backtrace*" "*Compilation*" "*Faces*" "*Help*" "*Messages*" "*Occur*" "*Warnings*" "*ag search"))
  "Regex that matches buffer names that should be displayed on the right side")

(defconst cv-side-buffer-store nil
  "Can hold a buffer that was displayed on the side, to enable toggling it.")

(defun cv-get-displayed-side-buffer ()
  "Returns buffer displayed on the side in case it matches `cv-side-window-rx'"
  (let* ((rw (car (window-at-side-list nil 'right)))
	 (bf (window-buffer rw))
	 (bn (buffer-name bf)))
    (when (string-match-p cv-side-window-rx bn) bf)))

(defun cv-1 ()
  "Maximizes left-most window, optionally storing a right side window if present."
  (interactive)
  (let ((bf (cv-get-displayed-side-buffer)))
    (when bf (setq cv-side-buffer-store bf)))
  (delete-other-windows))

(defun cv-2-1 ()
  "Changes window layout to two columns that are split two parts to one.
Assumes column-view layout."
  (interactive)
  (let* ((cw (count-windows))
	 (tc (round (* 0.667 (frame-total-cols)))))
    (cond 
     ((= 1 cw) ;; split window on the right
      (let* ((nw (split-window-right tc))
	     (ob (or cv-side-buffer-store (other-buffer))))
	(when ob (set-window-buffer nw ob))))
     
     ((= 2 cw) ;; grow left window
      (let* ((lw (car (window-at-side-list nil 'left))))
	(balance-windows)
	(window-resize lw (- tc (window-total-width lw)) 'horizontal)))
     
     ((= 3 cw) ;; delete middle window
      (let* ((lw (car (window-at-side-list nil 'left)))
	     (mw (window-in-direction 'right lw)))
	(delete-window mw)
	(balance-windows)
	(window-resize lw (- tc (window-total-width lw)) 'horizontal)))

     (t ;; maximize current window, like cw = 1
      (message "unexpected window count %s" cw)
      (let* ((nw (split-window-right tc))
	     (ob (or cv-side-buffer-store
		     (other-buffer))))
	(when ob (set-window-buffer nw ob))))
     )))

(defun cv-2 ()
  "Organizes windows into two evenly split columns."
  (interactive)
  (let* ((cw (count-windows)))
    (cond ((= 1 cw) ;; split window on the right
	   (let* ((nw (split-window-right))
		  (ob (or cv-side-buffer-store (other-buffer))))
	     (when ob (set-window-buffer nw ob))))

	  ((= 2 cw))

	  ((= 3 cw) ;; delete right window
	   (let ((rw (car (window-at-side-list nil 'right)))
		 (rb (window-buffer rw)))
	     (when (string-match-p cv-side-window-rx (buffer-name rb))
	       (setq cv-side-buffer-store rb))
	     (delete-window rw)))

	  (t ;; maximize current window, like cw  = 1
	   (message "unexpected window count %s" cw)
	   (delete-other-windows)
	   (let* ((nw (split-window-right))
		  (ob ( cv-side-buffer-store (other-buffer))))
	     (when ob (set-window-buffer nw ob)))))
    (balance-windows)))

(defun cv-3 ()
  "Organizes windows into three evenly split columns."
  (interactive)
  (let* ((cw (count-windows))
	 (ow (selected-window)))
    (cond ((= 1 cw) ;; split window on the right, twice
	   (let* ((nw (split-window-right))
		  (ob (other-buffer)))
	     (when ob (set-window-buffer nw ob))
	     (select-window nw)
	     (let* ((nnw (split-window-right))
		    (oob (or cv-side-buffer-store (other-buffer))))
	       (when oob (set-window-buffer nnw oob)))))

	  ((= 2 cw) ;; split in the middle or on right when no side buffer present
	   (let* ((br (cv-get-displayed-side-buffer))
		  (ob (or cv-side-buffer-store (other-buffer))))
	     (if br
		 (progn ;; split left to create a middle window
		   (select-window (car (window-at-side-list nil 'left)))
		   (let* ((mw (split-window-right))
			  (ob (other-buffer)))
		     (when ob (set-window-buffer mw ob))))
	       ;; split right
	       (select-window (car (window-at-side-list nil 'right)))
	       (let* ((rw (split-window-right))
		      (ob (or cv-side-buffer-store (other-buffer))))
		 (when ob (set-window-buffer rw ob))))))

	  ((= 3 cw)) ;; nothing to do here

	  (t ;; like cw = 1
	   (message "unexpected window count %s" cw)
	   (delete-other-windows)
	   (let* ((nw (split-window-right))
		  (ob (other-buffer)))
	     (when ob (set-window-buffer nw ob)))))
    (select-window ow)
    (balance-windows)))

(defun cv-toggle-side-window ()
  "Toggles the display of a side buffer
If there's a buffer displayed on the right whose name matches
`cv-side-window-rx', store it and delete the corresponding
window. Else if there is a stored buffer to display, display it
in the rightmost window."
  (interactive)
  (let* ((rb (cv-get-displayed-side-buffer))
	 (rw (car (window-at-side-list nil 'right))))
    (if rb
	;; store buffer and delete window
	(progn 
	  (setq cv-side-buffer-store rb)
	  (delete-window rw))
      ;; show stored buffer if present
      (when cv-side-buffer-store
	(let* ((nw (if (= 1 (count-windows))
		       (split-window-right)
		     (car (window-at-side-list nil 'right)))))
	(set-window-buffer nw cv-side-buffer-store)))
      )))

(defun cv-left-window ()
  "Select left-most window"
  (interactive)
  (select-window (car (window-at-side-list nil 'left))))

(defun cv-left-window-insert ()
  (interactive)
  (select-window (car (window-at-side-list nil 'left)))
  (global-modal-mode -1))

(defun cv-right-window ()
  "Select right-most window"
  (interactive)
  (select-window (car (window-at-side-list nil 'right))))

(defun cv-right-window-insert ()
  (interactive)
  (select-window (car (window-at-side-list nil 'right)))
  (global-modal-mode -1))

(defun cv-middle-window ()
  "Selects the second window from the left"
  (interactive)
  (select-window (car (window-at-side-list nil 'left)))
  (windmove-right))

(defun cv-middle-window-insert ()
  (interactive)
  (select-window (car (window-at-side-list nil 'left)))
  (windmove-right)
  (global-modal-mode -1))

(defun cv-display-buffer-right-side (buf alist)
  "Display buffer in right most window, split single window 2/1 if necessary"
  (let* ((cw (count-windows))
	 (tc (round (* 0.667 (frame-total-cols))))
	 (rw (car (window-at-side-list nil 'right))))
    (if (= 1 cw)
	(set-window-buffer (split-window-right tc) buf)
      (set-window-buffer rw buf))))
  
;; limit height of *Completions* buffer
(temp-buffer-resize-mode +1)
(setq temp-buffer-max-height 15)

(setq display-buffer-alist
      `(
	(,cv-side-window-rx (cv-display-buffer-right-side))

	(".*\\*Completions.*"
	 (display-buffer-reuse-window display-buffer-in-side-window)
	 (side . bottom)
	)))

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
