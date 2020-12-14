(install 'mark)
(install 'visual-regexp)

(defun increment-integer-at-point (&optional increment)
  (interactive "p*")
  (update-integer-at-point (lambda (num) (+ num (if increment increment 1)))))

(defun decrement-integer-at-point (&optional decrement)
  (interactive "p*")
  (update-integer-at-point (lambda (num) (- num (if decrement decrement 1)))))

(defun update-integer-at-point (update)
  (let ((offset (skip-chars-backward "0123456789")))
    (if (looking-at "[[:digit:]]+")
	(let* ((number-string (save-excursion
				(re-search-forward "[[:digit:]]+")
				(match-string 0)))
	       (should-pad-p (string-match "0+[[:digit:]]+" number-string))
	       (pad-number #'(lambda (num) (format (concat "%0" (number-to-string (length number-string)) "d") num)))
	       (number (string-to-number number-string))
	       (new-number (funcall update number))
	       (final-string (if should-pad-p (funcall pad-number new-number) (number-to-string new-number))))
	  (delete-region (point) (+ (point) (length number-string)))
	  (insert final-string)
	  (backward-char (+ (length number-string) offset)))
      (message "Can't identify number at point."))))

(defun insert-literal ()
  (interactive)
  (insert (read-string "Insert: ")))

(defun duplicate-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (push-mark (point))
    (end-of-line)
    (kill-ring-save (point) (mark))
    (open-line 1)
    (forward-char 1)
    (yank)))

(defun fg/open-line-below ()
  (interactive)
  (end-of-line)
  (open-line 1)
  (forward-line 1)
  (indent-for-tab-command)
  (global-modal-mode -1))

(defun fg/open-line-above ()
  (interactive)
  (end-of-line 0) ;; ie -1 🤷‍♂️
  (open-line 1)
  (forward-line 1)
  (indent-for-tab-command)
  (global-modal-mode -1))

(defun fg/replace-char ()
  (interactive)
  (let ((c (read-char "Replace with: ")))
    (mark-char)
    (delete-region (point) (mark))
    (insert c)))

(defun fg/insert-char ()
  (interactive)
  (let ((c (read-char "Insert: ")))
    (insert c)))

(defun find-next-left-pair-start ()
  (save-excursion
    (search-backward-regexp "\\((\\|{\\|\\[\\|<\\|'\\|\"\\|`\\)" (point-min) t)
    (let ((start-char (buffer-substring-no-properties (point) (1+ (point)))))
      (cond ((string= "(" start-char) '("(" . ")"))
	    ((string= "[" start-char) '("[" . "]"))
	    ((string= "{" start-char) '("{" . "}"))
	    ((string= "<" start-char) '("<" . ">"))
	    ((string= "'" start-char) '("'" . "'"))
	    ((string= "\"" start-char) '("\"" . "\""))
	    ((string= "`" start-char) '("`" . "`"))))))

(defun dispatch-with-pair (target &optional default)
  (let* ((last-key-seq (this-single-command-keys))
	 (last-key (elt last-key-seq (1- (length last-key-seq))))
	 (next-key (read-char "Pair start character: "))
	 (inner-most-pair (find-next-left-pair-start)))
    (cond ((= next-key ?\() (funcall target "(" ")"))
          ((= next-key ?\{) (funcall target "{" "}"))
          ((= next-key ?\[) (funcall target "[" "]"))
          ((= next-key ?\<) (funcall target "<" ">"))
          ((= next-key ?\') (funcall target "'" "'"))
          ((= next-key ?\") (funcall target "\"" "\""))
	  ((= next-key ?\`) (funcall target "`" "`"))
	  ((and (= next-key last-key) inner-most-pair)
	   (funcall target (car inner-most-pair) (cdr inner-most-pair)))
          (t
	   (when default (funcall default))
	   (pass-events (string next-key))))))

(defun remove-enclosing-pair ()
  (interactive)
  (dispatch-with-pair 'remove-enclosing-pair-strings))

(defun remove-enclosing-pair-strings (start end)
  (mark-inside-pair-strings start end)
  (let ((start-position (mark)))
    (delete-char (length end))
    (goto-char start-position)
    (delete-char (- (length start)))))

(defun enclose-in-pair ()
  (interactive)
  (unless (region-active-p) (mark-select))
  (dispatch-with-pair 'enclose-in-pair-strings))

(defun enclose-in-pair-strings (start end)
  (let* ((mark-position (mark))
         (point-position (point))
         (start-position (min mark-position point-position))
         (end-position (max mark-position point-position)))
    (goto-char end-position)
    (insert end)
    (goto-char start-position)
    (insert start)
    (goto-char (+ end-position (length end)))))

(defun identify-pair-at (pos)
  (save-excursion
    (goto-char pos)
    (cond
     ((looking-at "(")
      '("(" . ")"))
     ((looking-at "{")
      '("{" . "}"))
     ((looking-at "\\[")
      '("[" . "]"))
     ((looking-at "<")
      '("<" . ">"))
     ((looking-at "`")
      '("`" . "`"))
     ((looking-at "'")
      '("'" . "'"))
     ((looking-at "\"")
      '("\"" . "\"")))))

(defun find-first-preceding-pair-opener ()
  (save-excursion
    (re-search-backward "(\\|{\\|\\[\\|<\\|\"\\|'")))

(defun find-matching-unequal-closer (pos pair)
  (save-excursion
    (let* ((opener (car pair))
	   (closer (cdr pair))
	   (open-count 1))
      (goto-char (1+ pos))
      (while (and (not (eobp)) (< 0 open-count))
	(cond ((looking-at (regexp-quote opener)) (setq open-count (1+ open-count)))
	      ((looking-at (regexp-quote closer)) (setq open-count (1- open-count))))
	(forward-char 1))
      (unless (eobp) (1- (point))))))

(defun find-matching-equal-closer (pos pair)
  (let ((delimiter (car pair))
	closer-pos)
    (save-excursion
      (goto-char (1+ pos))
      (while (and (not closer-pos) (not (eobp)))
	(cond ((looking-at (regexp-quote (concat "\\" delimiter)))
	       (forward-char 2))
	      ((looking-at (regexp-quote delimiter))
	       (setq closer-pos (point)))
	      (t
	       (forward-char 1)))))
    closer-pos))

(defun find-matching-closer (pos)
  (let ((pair (identify-pair-at pos)))
    (when pair
      (if (string-equal (car pair) (cdr pair))
	  (find-matching-equal-closer pos pair)
	(find-matching-unequal-closer pos pair)))))

(defun surrounding-pair-info ()
  (let* ((start-pos (find-first-preceding-pair-opener))
	 (end-pos (find-matching-closer start-pos))
	 (pair (identify-pair-at start-pos)))
    `((:start-pos . ,start-pos) (:end-pos . ,end-pos) (:pair . ,pair))))

(defun slurp-forward ()
  (interactive)
  (let* ((info (surrounding-pair-info))
	 (end-pos (cdr (assoc :end-pos info))))
    (if (not (and info end-pos)) (message "couldn't find matching closer, info=%s." info)
      (save-excursion
	(goto-char (cdr (assoc :end-pos info)))
	(delete-char 1)
	(forward-symbol 1)
	(insert (cddr (assoc :pair info)))))))


;; ()abc
;; ()abc def ghi
;; (+ )abc
;; []abc
;; []abc def ghi
;; <html>abc
;; <html> abc def ghi
;; <html a="b"> abc def ghi
;; ""abc
;; "" abc
;; "abc " abc
;; "abc \" " abc
;; 'abc ""\' ' abc

(defun barf-forward ()
  (interactive)
  (let* ((info (surrounding-pair-info))
	 (end-pos (cdr (assoc :end-pos info))))
    (if (not (and info end-pos)) (message "couldn't find matching closer, info=%s." info)
      (save-excursion
	(goto-char (cdr (assoc :end-pos info)))
	(delete-char 1)
	(forward-symbol -1)
	(insert (cddr (assoc :pair info)))))))

(defun uuid ()
  (interactive)
  (let* ((raw (with-temp-buffer
		(shell-command "uuidgen" (current-buffer))
		(buffer-substring (point-min) (point-max))))
	 (uuid (progn (string-match "[ \t\n]*$" raw) (replace-match "" nil nil raw))))
    (insert uuid)))

(defun uid ()
  (interactive)
  (let* ((raw (with-temp-buffer
		(shell-command "uuidgen" (current-buffer))
		(buffer-substring (point-min) (point-max))))
	 (uuid (progn (string-match "[ \t\n]*$" raw) (replace-match "" nil nil raw))))
    (insert (substring uuid 0 8))))
