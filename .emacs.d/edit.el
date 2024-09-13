;; -*- lexical-binding: t -*-

(use-package mark)

(defun fg/change ()
  (interactive)
  (when (region-active-p)
	(kill-region (region-beginning) (region-end))
	(modal-mode-deactivate)))

(defun fg/surround-insert (first second) 
  (when (region-active-p)
	(let ((beg (region-beginning))
		  (end (region-end)))
	  (goto-char beg)
	  (insert first)
	  (goto-char (+ end 1))
	  (insert second)
	  ;; re-activate region
	  (setq deactivate-mark nil)
	  (forward-char -1)
	  (set-mark (point))
	  (goto-char (+ beg 1))
	  (exchange-point-and-mark)
)))

(defun fg/surround () 
  (interactive)
  (unless (region-active-p) (mark-symbol))
  (let* ((char (read-char "pair character: "))
		 (pair (fg/pair-for-char char)))
	(if pair (fg/surround-insert (car pair) (cdr pair))
	  (message "unknown pair %c" char))))

(defconst fg/pairs '(
					 (?\( . ?\))
					 (?\{ . ?\})
					 (?\[ . ?\])
					 (?\" . ?\")
					 (?\' . ?\')
					 (?\< . ?\>)
					 (?\` . ?\`)
))

(defun fg/pair-for-char (ch) 
  (let ((by-car (assoc ch fg/pairs)))
	(if by-car by-car
	  (rassoc ch fg/pairs))))

(defun fg/surround-remove () 
  (interactive)
  (when (region-active-p)
	(let ((beg (region-beginning))
		  (end (region-end)))
	  (goto-char end)
	  (message "looking at %c" (char-after (point)))
	  (delete-char 1)
	  (goto-char (- beg 1))
	  (delete-char 1)
	  
	  ;; re-activate region
	  (setq deactivate-mark nil)
	  (set-mark (point))
	  (goto-char (- end 2))
)))

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

(defun fg/insert-literal ()
  (interactive)
  (insert (read-string "Insert: ")))

(defun fg/duplicate-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (push-mark (point))
    (end-of-line)
    (forward-char 1)
    (kill-ring-save (point) (mark))
    (yank)))

(defun fg/delete ()
  (interactive)
  (if (region-active-p)
	  (kill-region (point) (mark))
	(delete-forward-char 1)))

(defun fg/join-line ()
  (interactive)
  (save-excursion
	(end-of-line)
	(delete-char 1)
	(fixup-whitespace)))

(defun fg/kill-line ()
  (interactive)
  (let ((col (current-column)))
	(save-excursion
      (beginning-of-line)
      (push-mark (point))
      (end-of-line)
	  (forward-char 1)
      (kill-region (point) (mark))
	  (message "moving to column %s" col))
	(move-to-column col)))

(defun fg/end-of-line-insert ()
  (interactive)
  (end-of-line)
  (global-modal-mode -1))

(defun fg/beginning-of-line-insert ()
  (interactive)
  (beginning-of-line)
  (global-modal-mode -1))

(defun fg/open-line-below ()
  (interactive)
  (end-of-line)
  (open-line 1)
  (forward-line 1)
  (indent-for-tab-command))

(defun fg/open-line-below-insert ()
  (interactive)
  (fg/open-line-below)
  (global-modal-mode -1))

(defun fg/open-line-above ()
  (interactive)
  (end-of-line 0) ;; ie -1 🤷‍
  (open-line 1)
  (forward-line 1)
  (indent-for-tab-command))

(defun fg/open-line-above-insert ()
  (interactive)
  (fg/open-line-above)
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

(defun fg/kill-select-save ()
  (interactive)
  (fg/kill-select 'save))

(defun fg/kill-select (&optional save)
  (interactive)
  (let* (mrk-fun)
    (unless (region-active-p)
      (let* ((help-form region-specifier-help)
			 (nk (read-char "Kill: ")))
		(cond
		 ((region-active-p) t)
		 ((= nk (region-specifier 'char)) (setq mrk-fun 'mark-char))
		 ((= nk (region-specifier 'line)) (setq mrk-fun 'mark-whole-line))
		 ((= nk (region-specifier 'line-rest)) (setq mrk-fun 'mark-until-end-of-line))
		 ((= nk (region-specifier 'word)) (setq mrk-fun 'mark-word))
		 ((= nk (region-specifier 'symbol)) (setq mrk-fun 'mark-symbol))
		 ((= nk (region-specifier 'inside-pair)) (setq mrk-fun 'mark-inside-pair))
		 ((= nk (region-specifier 'with-pair)) (setq mrk-fun 'mark-with-pair))
		 ((= nk (region-specifier 'whitespace)) (setq mrk-fun 'mark-whitespace))
		 ((= nk (region-specifier 'till)) (setq mrk-fun 'mark-till))
		 ((= nk (region-specifier 'till-backwards)) (setq mrk-fun 'mark-till-backwards)))))
    (when mrk-fun (funcall mrk-fun))
	(if save
		(kill-ring-save (point) (mark))
      (kill-region (point) (mark)))
    (setq fg/last-edit-command (lambda () (fg/kill-select-do mrk-fun)))))

(defun fg/kill-select-do (mrk-fun)
  (funcall mrk-fun)
  (kill-region (point) (mark)))

(defun kill-till ()
  (mark-till)
  (kill-region (point) (mark)))

(defun kill-till-backwards ()
  (mark-till-backwards)
  (kill-region (point) (mark)))

(defun kill-char ()
  (mark-char)
  (kill-region (point) (mark)))

(defun kill-word ()
  (mark-word)
  (kill-region (point) (mark)))

(defun kill-symbol ()
  (mark-symbol)
  (kill-region (point) (mark)))

(defun kill-until-end-of-line ()
  (mark-until-end-of-line)
  (kill-region (point) (mark)))

(defun kill-whole-line ()
  (mark-whole-line)
  (kill-region (point) (mark))
  (delete-char 1))

(defun kill-inside-pair ()
  (mark-inside-pair)
  (kill-region (point) (mark)))

(defun kill-with-pair ()
  (mark-with-pair)
  (kill-region (point) (mark)))

(defun kill-whitespace ()
  (mark-whitespace)
  (kill-region (point) (mark)))

(defconst fg/last-edit-command nil "lambda that can be evaluated to repeat last edit command")

(defun fg/repeat-last-edit ()
  (interactive)
  (when fg/last-edit-command
    (funcall fg/last-edit-command)))

(defun fg/replace-select ()
  (interactive)
  (let* (mrk ins)
    (unless (region-active-p)
      (let* ((help-form region-specifier-help)
			 (nk (read-char "Mark: "))) ;; TODO don't need this when region is active
		(cond
		 ((= nk (region-specifier 'char)) (setq mrk 'mark-char))
		 ((= nk (region-specifier 'line)) (setq mrk 'mark-whole-line))
		 ((= nk (region-specifier 'line-rest)) (setq mrk 'mark-until-end-of-line))
		 ((= nk (region-specifier 'word)) (setq mrk 'mark-word))
		 ((= nk (region-specifier 'symbol)) (setq mrk 'mark-symbol))
		 ((= nk (region-specifier 'inside-pair)) (setq mrk 'mark-inside-pair))
		 ((= nk (region-specifier 'with-pair)) (setq mrk 'mark-with-pair))
		 ((= nk (region-specifier 'whitespace)) (setq mrk 'mark-whitespace))
		 ((= nk (region-specifier 'till)) (setq mrk 'mark-till))
		 ((= nk (region-specifier 'till-backwards)) (setq mrk 'mark-till-backwards))
		 (t (setq mrk 'mark-whole-line)))
		(funcall mrk)))
    (setq ins (read-string "Replace with: "))
    (delete-region (point) (mark))
    (insert ins)
    (setq fg/last-edit-command (lambda () (fg/replace-select-do mrk ins)))))

(defun fg/replace-select-do (mrk ins)
  (funcall mrk)
  (delete-region (point) (mark))
  (insert ins))

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
  (unless (region-active-p) (fg/mark-select))
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
