;; TODO xF to open in window 2
;; TODO K to open in window 2
;; TODO xref in right or middle window
;; TODO do we need to call set-window-buffer

(defun cv-create-right-side-window ()
  (let* ((old-win (selected-window))
	 (quit-restore (window-parameter old-win 'quit-restore))
	 (right-win (car (window-at-side-list nil 'right)))
	 (new-win (split-window right-win nil 'right)))
    (set-window-parameter new-win 'quit-restore quit-restore)
    (set-window-parameter new-win 'window-side 'right)
    new-win))

(defun cv-get-right-side-window ()
  (let* ((right-win (car (window-at-side-list nil 'right)))
	 (is-side-win (eq 'right (window-parameter right-win 'window-side))))
    (when is-side-win right-win)))

(defun cv-1 ()
  (interactive)
  (let ((rw (cv-get-right-side-window)))
    (when (and rw (not cv-side-window-store))
      (setq cv-side-window-store (window-buffer rw))))
  (delete-other-windows))

(defun cv-2 ()
  (interactive)
  (let* ((cw (count-windows)))
    (cond ((= 1 cw)
	   (let* ((nw (split-window-right))
		  (ob (other-buffer)))
	     (when ob (set-window-buffer nw ob))))

	  ((= 2 cw)	   ;; nothing to do here
	   )

	  ((= 3 cw)
	   (let ((rw (cv-get-right-side-window)))
	     (when (and rw (not cv-side-window-store))
	       (setq cv-side-window-store (window-buffer rw))))
	   (delete-window (car (window-at-side-list nil 'right))))

	  (t
	   (message "unexpected window count %s" cw)
	   (delete-other-windows)
	   (let* ((nw (split-window-right))
		  (ob (other-buffer)))
	     (when ob (set-window-buffer nw ob))))))
  (balance-windows))

(defun cv-3 ()
  (interactive)
  (let* ((cw (count-windows)))
    (cond ((= 1 cw)
	   (let* ((nw (split-window-right))
		  (ob (other-buffer)))
	     (when ob (set-window-buffer nw ob))
	     (let* ((nnw (split-window-right))
		    (oob (other-buffer)))
	       (when oob (set-window-buffer nnw oob)))))

	  ((= 2 cw)
	   (let* ((rw (cv-get-right-side-window))
		  (ws (car (window-at-side-list nil (if rw 'left 'right))))
		  (ob (other-buffer))
		  nw)
	     (setq nw (split-window ws nil 'right))
	     (when ob (set-window-buffer nw ob))))

	  ((= 3 cw) ;; nothing to do here
	   )

	  (t
	   (message "unexpected window count %s" cw)
	   (delete-other-windows)
	   (let* ((nw (split-window-right))
		  (ob (other-buffer)))
	     (when ob (set-window-buffer nw ob))))))

  (balance-windows (selected-frame)))

(defconst cv-side-window-store nil)

(defun cv-toggle-right-side-window ()
  (interactive)
  (let* ((rw (cv-get-right-side-window)))
    (if rw
	(progn
	  (message "found right side-win %s" rw)
	  (setq cv-side-window-store (window-buffer rw))
	  (delete-window rw)
	  (balance-windows))
      (if cv-side-window-store
	  (progn
	    (message "found stored side-win %s" cv-side-window-store)
	    (cv-display-buffer-right-side cv-side-window-store nil))
	(message "found no live or stored side-window")
	))))

(defun cv-left-window ()
  (interactive)
  (select-window (car (window-at-side-list nil 'left))))

(defun cv-left-window-insert ()
  (interactive)
  (select-window (car (window-at-side-list nil 'left)))
  (global-modal-mode -1))

(defun cv-right-window ()
  (interactive)
  (select-window (car (window-at-side-list nil 'right))))

(defun cv-right-window-insert ()
  (interactive)
  (select-window (car (window-at-side-list nil 'right)))
  (global-modal-mode -1))

(defun cv-middle-window ()
  (interactive)
  (select-window (car (window-at-side-list nil 'left)))
  (windmove-right))

(defun cv-middle-window-insert ()
  (interactive)
  (select-window (car (window-at-side-list nil 'left)))
  (windmove-right)
  (global-modal-mode -1))

(defun cv-display-buffer-right-side (buf alist)
  (let* ((cw (count-windows))
	 win)
    (setq cw (count-windows))
    (cond ((= 1 cw)
	   (setq win (cv-create-right-side-window)))
	  ((= 2 cw)
	   (let* ((right-win (car (window-at-side-list nil 'right)))
		  (is-side-win (eq 'right (window-parameter right-win 'window-side))))
	     (setq win
		   (if is-side-win
		       right-win
		     (cv-create-right-side-window)))))
	  ((= 3 cw)
	   (let* ((right-win (car (window-at-side-list nil 'right)))
		  (is-side-win (eq 'right (window-parameter right-win 'window-side))))
	     (setq win
		   (if is-side-win
		       right-win
		     (delete-window rightwin)
		     (cv-create-right-side-window)))))
	  (t
	   (message "unexpected window count %s" cw)
	   (setq win (selected-window))))
    (set-window-buffer win buf)
    (balance-windows)))

;; (defun cv-find-file-second-column (file-name &optional wildcards)
;;   (interactive
;;    (find-file-read-args "Find file in second column: "
;;                         (confirm-nonexistent-file-or-buffer)))
;;   (let ((buf (find-file-noselect file-name nil nil wildcards)))


(setq display-buffer-alist
      `((,(rx bos (or "*Help*" "*Compilation*" "*Warnings*" "*Messages*" "*Faces*" "*Backtrace*"))
	 (cv-display-buffer-right-side))))


(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
