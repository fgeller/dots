(defconst region-specifiers
  '((char . ?c)
    (line . ?l)
    (line-rest . ?L)
    (word . ?w)
    (symbol . ?s)
    (whitespace . ? )
    (inside-pair . ?p)
    (with-pair . ?P))
  "Mapping from region type to identifier key")

(defconst region-specifier-help "
The following region specifiers are availabe:

key    region
---    ------

  c    char
  l    line
  L    line rest
  w    word
  s    symbol
       whitespace
  p    inside-pair
  P    with-pair

")

(defun region-specifier (type)
  (cdr (assoc type region-specifiers)))

(defun kill-select ()
  (interactive)
  (cond ((region-active-p) (kill-region (point) (mark)))
	(t
	 (let* ((help-form region-specifier-help)
		(nk (read-char "Kill: ")))
	   (cond
	    ((= nk (region-specifier 'char)) (kill-char))
	    ((= nk (region-specifier 'line)) (kill-whole-line))
	    ((= nk (region-specifier 'line-rest)) (kill-until-end-of-line))
	    ((= nk (region-specifier 'word)) (kill-word))
	    ((= nk (region-specifier 'symbol)) (kill-symbol))
	    ((= nk (region-specifier 'inside-pair)) (kill-inside-pair))
	    ((= nk (region-specifier 'with-pair)) (kill-with-pair))
	    ((= nk (region-specifier 'whitespace)) (kill-whitespace))
	    ((/= nk help-char) (push-mark (point))
	     (call-interactively (key-binding (kbd (string nk))))
	     (kill-region (point) (mark))))))))

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

(defun replace-select ()
  (interactive)
  (unless (region-active-p)
    (let* ((help-form region-specifier-help)
	   (nk (read-char "Mark: ")))
      (cond
       ((= nk (region-specifier 'char)) (mark-char))
       ((= nk (region-specifier 'line)) (mark-whole-line))
       ((= nk (region-specifier 'line-rest)) (mark-until-end-of-line))
       ((= nk (region-specifier 'word)) (mark-word))
       ((= nk (region-specifier 'symbol)) (mark-symbol))
       ((= nk (region-specifier 'inside-pair)) (mark-inside-pair))
       ((= nk (region-specifier 'with-pair)) (mark-with-pair))
       ((= nk (region-specifier 'whitespace)) (mark-whitespace))
       (t (mark-word))))) ; defaults to word
  (let ((rp (read-string "Replace with: ")))
    (delete-region (point) (mark))
    (insert rp)))

(defun mark-select ()
  (interactive)
  (if (region-active-p) (kill-ring-save nil nil 'region)
    (let* ((help-form region-specifier-help)
	   (nk (read-char "Mark: ")))
      (cond
       ((= nk (region-specifier 'char)) (mark-char))
       ((= nk (region-specifier 'line)) (mark-whole-line))
       ((= nk (region-specifier 'line-rest)) (mark-until-end-of-line))
       ((= nk (region-specifier 'word)) (mark-word))
       ((= nk (region-specifier 'symbol)) (mark-symbol))
       ((= nk (region-specifier 'inside-pair)) (mark-inside-pair))
       ((= nk (region-specifier 'with-pair)) (mark-with-pair))
       ((= nk (region-specifier 'whitespace)) (mark-whitespace))
       (t (set-mark (point))
	  (pass-events (string nk)))))))

(defun mark-char ()
  (set-mark (point))
  (forward-char 1))

(defun mark-word ()
  (unless (looking-at-word-p) (beginning-of-word))
  (set-mark (point))
  (forward-word))

(defun mark-symbol ()
  (unless (looking-at-symbol-p) (beginning-of-symbol))
  (set-mark (point))
  (forward-symbol 1))

(defun mark-until-end-of-line ()
  (set-mark (point))
  (end-of-line))

(defun mark-whole-line ()
  (beginning-of-line)
  (set-mark (point))
  (end-of-line))

(defun mark-inside-pair ()
  (dispatch-with-pair 'mark-inside-pair-strings
                      (lambda () (push-mark (point)))))

(defun move-point-to-balanced-start (start end)
  (move-point-to-balanced t start end))

(defun move-point-to-balanced-end (start end)
  (move-point-to-balanced nil start end))

(defun move-point-to-balanced (look-for-start start end)
  (let ((counter 1))
    (while (> counter 0)
      (if look-for-start (backward-char 1) (forward-char 1))
      (cond ((looking-at (regexp-quote (if look-for-start end start))) (setq counter (1+ counter)))
            ((looking-at (regexp-quote (if look-for-start start end))) (setq counter (1- counter)))))))

(defun move-point-to-pair-start-simple (pair)
  (backward-char 1)
  (while (not (looking-at (regexp-quote pair)))
    (backward-char 1)))

(defun move-point-to-pair-end-simple (pair)
  (forward-char 1)
  (while (not (looking-at (regexp-quote pair)))
    (forward-char 1)))

(defun move-point-to-pair-starting-string (start end)
  (if (string= start end)
      (move-point-to-pair-start-simple start)
    (move-point-to-balanced-start start end)))

(defun move-point-to-pair-ending-string (start end)
  (if (string= start end)
      (move-point-to-pair-end-simple start)
    (move-point-to-balanced-end start end)))

(defun mark-inside-pair-strings (start end)
  (move-point-to-pair-starting-string start end)
  (forward-char 1)
  (push-mark (point))
  (backward-char 1)
  (move-point-to-pair-ending-string start end))

(defun mark-with-pair ()
  (dispatch-with-pair 'mark-with-pair-strings))

(defun mark-whitespace ()
  (interactive)
  (skip-chars-backward " \t")
  (push-mark (point))
  (skip-chars-forward " \t"))

(defun mark-with-pair-strings (start end)
  (move-point-to-pair-starting-string start end)
  (push-mark (point))
  (move-point-to-pair-ending-string start end)
  (forward-char 1))

(defun pass-events (kbd-string)
  "Helper to pass keyboard events through to shadowed maps. Based on `boon-push-events'"
  (setq unread-command-events
        (append (kbd kbd-string) unread-command-events)))

(provide 'mark)
