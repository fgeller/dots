(defconst 3w-minimal-window-width 80
  "Minimal width of a single window. This is used to determined
  if 3w should use a horizontal or vertical split. e.g. if the
  minimal width of a window is 60 and the available width is 170
  columns, then 3w will choose a vertical layout as 170 columns
  cannot support three vertically split windows with a minimal
  width of 60 columns.")

(defconst 3w-side-buffer-store nil
  "Can hold a side-window's buffer")

(defconst 3w-side-window-rx (rx (or
				 "*Backtrace*"
				 "*Compilation*"
				 "*Faces*"
				 "*Help*"
				 "*Messages*"
				 "*Occur*"
				 "*Warnings*"
				 "*ag search"
				 ))
  "Regex that matches buffer names that should be displayed in a side window")

(defun 3w-get-side-window-buffer ()
  "Returns buffer displayed in side window if it matches `3w-side-window-rx'"
  (let* ((is-cs (3w-is-column-split-p))
	 (sw (car (window-at-side-list nil (if is-cs 'right 'bottom)))))
    (when (3w-side-window-name-p sw) (window-buffer sw))))

(defun 3w-store-side-buffer (&optional buf)
  (let* ((sb (or buf (3w-get-side-window-buffer))))
    (when sb
      (setq 3w-side-buffer-store sb)
      ;; (message "3w: stored side window buffer %s" sb)
      )))

(defun 3w-side-window-name-p (w)
  (3w-side-buffer-name-p (window-buffer w)))

(defun 3w-side-buffer-name-p (b)
  (string-match-p 3w-side-window-rx (buffer-name b)))

(defun 3w-is-column-split-p ()
  (> (window-size) (- (frame-total-lines) 5)))

(defun 3w-should-split-into-columns-p (count)
  "Indicates whether there is enough space for 3w to split windows into columns"
  (> (frame-total-cols)
     (* count 3w-minimal-window-width)))

(defun 3w-split-window (count &optional size)
  (if (3w-should-split-into-columns-p count)
      (split-window-right size)
    (split-window-below size)))

(defun 3w-split-1 ()
  "Maximizes current window, optionally storing a side window's buffer if present"
  (interactive)
  (3w-store-side-buffer)
  (delete-other-windows))

(defun 3w-split-2 ()
  "Produces two evenly split windows"
  (interactive)
  (let* ((should-cs (3w-should-split-into-columns-p 2))
	 (size (round (/ (if should-cs (frame-total-cols) (frame-total-lines)) 2))))
    (3w-split-2-with-size size)
    (balance-windows)))

(defun 3w-split-2-1 ()
  "Produces two split windows where the left/top window is 2/3 of the frame size"
  (interactive)
  (let* ((should-cs (3w-should-split-into-columns-p 2))
	 (size (round (* 0.667 (if should-cs (frame-total-cols) (frame-total-lines)) ))))
    (3w-split-2-with-size size)))

(defun 3w-split-2-with-size-from-1 (size &optional next-buffer)
  (delete-other-windows)
  (let* ((nb (or next-buffer 3w-side-buffer-store (other-buffer)))
	 (nw (3w-split-window 2 size)))
    (set-window-buffer nw nb)))
   
(defun 3w-split-2-with-size (size)
  "Produces two split windows with the given size for the right/bottom window"
  (3w-store-side-buffer)
  (let* ((wc (count-windows)))
    (cond ((= 1 wc)
	   (3w-split-2-with-size-from-1 size))
	  ((= 2 wc)
	   (3w-split-2-with-size-from-1 size (window-buffer (next-window))))
	  ((= 3 wc)
	   (3w-split-2-with-size-from-1 size (window-buffer (next-window))))
	  (t
	   (3w-split-2-with-size-from-1 size)))))

(defun 3w-split-3-from-1 (&optional next-buffer next-next-buffer)
  ;; (message "-from-1: nb=%s nnb=%s" next-buffer next-next-buffer)
  (3w-store-side-buffer)
  (delete-other-windows)
  (let* ((nb (or next-buffer (other-buffer)))
	 (nnb (or next-next-buffer
		  (unless (eq nb 3w-side-buffer-store) 3w-side-buffer-store)
		  (other-buffer nb)))
	 (ow (selected-window))
	 (nw (3w-split-window 3))
	 (nnw (progn (select-window nw) (3w-split-window 3))))
    (set-window-buffer nw nb)
    (set-window-buffer nnw nnb)
    (select-window ow)
    (balance-windows)))

(defun 3w-split-3 ()
  "Produces three evenly split windows"
  (interactive)
  (let* ((wc (count-windows))
	 (nb (window-buffer (next-window)))
	 (nnb (window-buffer (next-window (next-window)))))
    (balance-windows)
    (cond ((= 1 wc)
	   (3w-split-3-from-1))
	  ((= 2 wc)
	   (3w-split-3-from-1 nb))
	  ((= 3 wc)
	   (3w-split-3-from-1 nb nnb))
	  (t
	   (3w-split-3-from-1)))))

(defun 3w-jump-1 ()
  (interactive)
  (select-window
   (car (window-at-side-list nil (if (3w-is-column-split-p) 'left 'top)))))

(defun 3w-jump-2 ()
  (interactive)
  (let* ((is-cs (3w-is-column-split-p)))
  (select-window (car (window-at-side-list nil (if is-cs 'left 'top))))
  (if is-cs (windmove-right) (windmove-down))))

(defun 3w-jump-3 ()
  (interactive)
  (select-window
   (car (window-at-side-list nil (if (3w-is-column-split-p) 'right 'bottom)))))

(defun 3w-display-as-side-window (buf &optional alist)
  "Display buffer in right or bottom window, split single window if necessary"
  ;; (message "3w-display-as-side-window buf=%s" buf)
  (when (= 1 (count-windows))
    (3w-split-2-1))
  (let* ((is-cs (3w-is-column-split-p))
	 (sw (car (window-at-side-list nil (if is-cs 'right 'bottom)))))
    ;; (message "3w-display-as-side-window: is-cs=%s sw=%s buf=%s" is-cs sw buf)
    (set-window-buffer sw buf)))

(defun 3w-toggle-side-window ()
  (interactive)
  (let* ((is-cs (3w-is-column-split-p))
	 (sw (car (window-at-side-list nil (if is-cs 'right 'bottom))))
	 (cw (count-windows)))
    (cond ((= 1 cw)
	   (if 3w-side-buffer-store
	       (3w-display-as-side-window 3w-side-buffer-store)
	     ;; (message "3w: no side window buffer in store")
	     ))

	  ((3w-side-window-name-p sw)
	   (3w-store-side-buffer (window-buffer sw))
	   (delete-window sw)
	   (3w-split-2))

	  (3w-side-buffer-store
	   (if (= cw 3)
	       (set-window-buffer sw 3w-side-buffer-store)
	     (3w-split-3)))

	  (t
	   (message "3w: unexpected case for 3w-toggle-side-window store=%s sw=%s cw=%s"
		    3w-side-buffer-store sw cw)))))

(defconst 3w-map (make-keymap) "Keymap to bind to a prefix to conveniently access 3w's and built-in functions")

(define-key 3w-map (kbd "1") '3w-split-1)
(define-key 3w-map (kbd "2") '3w-split-2)
(define-key 3w-map (kbd "3") '3w-split-3)
(define-key 3w-map (kbd "4") '3w-split-2-1)
(define-key 3w-map (kbd "0") 'delete-window)
(define-key 3w-map (kbd "d") 'delete-window)
(define-key 3w-map (kbd "b") 'balance-windows)
(define-key 3w-map (kbd "a") '3w-jump-1)
(define-key 3w-map (kbd "s") '3w-jump-2)
(define-key 3w-map (kbd "h") '3w-jump-3)
(define-key 3w-map (kbd "t") '3w-toggle-side-window)
(define-key 3w-map (kbd "o") 'other-window)

;;
;; Tests
;;

(ert-deftest 3w-1->2c->1 ()
  (3w-test-fixture
   (lambda ()
     (delete-other-windows)
     (let* ((3w-minimal-window-width 50)
	    (ow (selected-window)))
       (set-frame-size nil 100 50)
       (3w-split-2)
       (should (= 2 (count-windows)))
       (should (<= 48 (window-width) 50))
       (windmove-right)
       (should-not (eq ow (selected-window)))
       (should (<= 48 (window-width) 50))
       (windmove-left)
       (3w-split-1)
       (should (= 1 (count-windows)))
       (should (eq ow (selected-window)))))))

(ert-deftest 3w-1->2c->3r ()
  (3w-test-fixture
   (lambda ()
     (delete-other-windows)
     (let* ((3w-minimal-window-width 50)
	    (ow (selected-window)))
       (set-frame-size nil 100 50)
       (3w-split-2)
       (should (= 2 (count-windows)))
       (should (<= 48 (window-width) 50))
       (3w-split-3)
       (should (= 3 (count-windows)))
       (should (eq ow (selected-window)))
       (should (<= 98 (window-width) 100))
       (should (<= (1- (/ 50 3)) (window-height) (1+ (/ 50 3))))
       ))))

(ert-deftest 3w-1->2r->1 ()
  (3w-test-fixture
   (lambda ()
     (delete-other-windows)
     (let* ((3w-minimal-window-width 50)
	    (ow (selected-window)))
       (set-frame-size nil 80 50)
       (3w-split-2)
       (should (= 2 (count-windows)))
       (should (<= 78 (window-width) 80))
       (should (<= 24 (window-height) 25))
       (windmove-down)
       (should-not (eq ow (selected-window)))
       (should (<= 78 (window-width) 80))
       (should (<= 24 (window-height) 25))
       (windmove-up)
       (3w-split-1)
       (should (= 1 (count-windows)))
       (should (eq ow (selected-window)))))))

(defun 3w-test-fixture (body)
  (let* (fh fw)
    (unwind-protect
	(progn
	  (setq fw (frame-width))
	  (setq fh (frame-height))
	  (funcall body))
      (set-frame-size nil fw fh))))

;; (ert "^3w-")

(provide '3w)
