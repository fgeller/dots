(defconst fg/mark-active-p nil)
(defun fg/deactivate-mark-hook () (setq fg/mark-active-p nil))
(add-hook 'deactivate-mark-hook 'fg/deactivate-mark-hook)

(defun fg/activate-mark () 
  (interactive)
  (setq fg/mark-active-p 't)
  (set-mark (point)))

(defun fg/back-node-or-word ()
  (interactive)
  (backward-word)
  (forward-word)
  (unless fg/mark-active-p (set-mark (point)))
  (backward-word))

(defun fg/forward-node-or-word ()
  (interactive)
  (forward-word)
  (backward-word)
  (unless fg/mark-active-p (set-mark (point)))
  (forward-word))

(defun fg/move-end-of-line ()
  (interactive)
  (move-end-of-line 1))

(defun fg/move-beginning-of-line ()
  (interactive)
  (move-beginning-of-line 1))

(defun fg/next-line ()
  (interactive)
  (when (and (not fg/mark-active-p)
			 (region-active-p))
	(deactivate-mark))
  (next-line))

(defun fg/previous-line ()
  (interactive)
  (when (and (not fg/mark-active-p)
			 (region-active-p))
	(deactivate-mark))
  (previous-line))

(defun fg/left-char ()
  (interactive)
  (when (and (not fg/mark-active-p)
			 (region-active-p))
	(deactivate-mark))
  (left-char))

(defun fg/right-char ()
  (interactive)
  (when (and (not fg/mark-active-p)
			 (region-active-p))
	(deactivate-mark))
  (right-char))

(defun fg/backward-symbol ()
  (interactive)
  (forward-symbol -1)
  (forward-symbol 1)
  (unless fg/mark-active-p (set-mark (point)))
  (forward-symbol -1))

(defun fg/forward-symbol ()
  (interactive)
  (forward-symbol 1)
  (forward-symbol -1)
  (unless fg/mark-active-p (set-mark (point)))
  (forward-symbol 1))

(defun fg/down-node-or-scroll ()
  (interactive)
  (fg/scroll-up-half-page))

(defun fg/up-node-or-scroll ()
  (interactive)
  (fg/scroll-down-half-page))

(defun fg/jump-to-char ()
  (interactive)
  (avy-goto-char-timer))
   
(defun fg/beginning-of-buffer ()
  (interactive)
  (beginning-of-buffer))

(defun fg/end-of-buffer ()
  (interactive)
  (end-of-buffer))

(defun fg/forward-to-char ()
  (interactive)
  (let* ((tc (read-char "jump to:")))
    (right-char 1)
    (search-forward (char-to-string tc))
    (left-char 1)))

(defun fg/backward-to-char ()
  (interactive)
  (let* ((tc (read-char "jump to:")))
    (left-char 1)
    (search-backward (char-to-string tc))))

(defun fg/jump-to-matching-paren ()
  (interactive)
  (cond ((looking-at "\\s(") (forward-sexp 1))
        ((looking-back "\\s)" 1) (backward-sexp 1))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp 1))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp 1))))

(defun fg/consult-goto-line ()
  (interactive)
  (call-interactively 'goto-line))

(defun fg/consult-line ()
  (interactive)
  (consult-line))

(defun fg/grep ()
  (interactive)
  (consult-git-grep))

(defun fg/rg ()
  (interactive)
  (consult-ripgrep))

(defun fg/jump ()
  (interactive)
  (consult-buffer))

(defun fg/imenu ()
  (interactive)
  (consult-imenu))

(defun bol-modal-mode-deactivate ()
  (interactive)
  (beginning-of-line)
  (modal-mode-deactivate))

(defun after-indent-modal-mode-deactivate ()
  (interactive)
  (back-to-indentation)
  (modal-mode-deactivate))

(defun eol-modal-mode-deactivate ()
  (interactive)
  (end-of-line)
  (modal-mode-deactivate))

(defun fg/save-point ()
  (interactive)
  (point-to-register ?p))

(defun fg/restore-point ()
  (interactive)
  (jump-to-register ?p))

(defun fg/scroll-down-half-page ()
  (interactive)
  (scroll-down-command (/ (window-height) 4))
  (pulse-momentary-highlight-one-line (point)))

(defun fg/scroll-up-half-page ()
  (interactive)
  (scroll-up-command (/ (window-height) 4))
  (pulse-momentary-highlight-one-line (point)))

(defun beginning-of-symbol ()
  (while (not (looking-at-symbol-p))
    (left-char 1)))

(defun looking-at-symbol-p ()
  (looking-at "\\_<"))

(defun looking-at-word-p ()
  (if subword-mode
      (or (looking-at "\\b")
          (let ((pos-start (point)) pos-after)
            (save-excursion
              (subword-forward -1)
              (subword-forward 1)
              (setq pos-after (point)))
            (= pos-start pos-after)))
    (looking-at "\\b")))

(defun beginning-of-word ()
  (while (not (looking-at-word-p))
    (left-char 1)))

(defun fg/move-to-next-symbol-occurrence ()
  (interactive)
  (beginning-of-symbol)
  (forward-symbol 1)
  (let ((thing (thing-at-point 'symbol)))
    (setq isearch-string thing)
    (search-forward-regexp (concat "\\_<" (regexp-quote thing) "\\_>")))
  (beginning-of-symbol)
  (save-excursion
    (forward-symbol 1)
    (push-mark (point))))

(defun fg/move-to-previous-symbol-occurrence ()
  (interactive)
  (beginning-of-symbol)
  (let ((thing (thing-at-point 'symbol)))
    (setq isearch-string thing)
    (search-backward-regexp (concat "\\_<" (regexp-quote thing) "\\_>"))
    (save-excursion
      (forward-symbol 1)
      (push-mark (point)))))

(defun fg/move-to-next-word-occurrence ()
  (interactive)
  (beginning-of-word)
  (forward-word)
  (let ((thing (thing-at-point 'word)))
    (setq isearch-string thing)
    (search-forward-regexp (concat "\\<" (regexp-quote thing) "\\>")))
  (beginning-of-word)
  (save-excursion
    (forward-word)
    (push-mark (point))))

(defun fg/move-to-previous-word-occurrence ()
  (interactive)
  (beginning-of-word)
  (let ((thing (thing-at-point 'word)))
    (setq isearch-string thing)
    (search-backward-regexp (concat "\\<" (regexp-quote thing) "\\>"))
    (save-excursion
      (forward-word)
      (push-mark (point)))))

(defun compilation-goto-first-error ()
  (interactive)
  (with-current-buffer "*compilation*"
    (goto-char (point-min))
    (compilation-next-error 1)
    (compile-goto-error)))

(defun compilation-goto-next-error ()
  (interactive)
  (with-current-buffer "*compilation*"
    (compilation-next-error 1)
    (compile-goto-error)))

(defconst fg/last-fg-command nil)
(defconst fg/last-search-char nil)

(defun fg/forward-char ()
  (interactive)
  (setq fg/last-fg-command 'fg/forward-char)
  (let ((ch (if fg/last-search-char fg/last-search-char
              (setq fg/last-search-char (string (read-char "char: "))))))
    (unless (region-active-p) (set-mark (point)))
    (when (looking-at ch) (forward-char 1))
    (search-forward ch nil 't)
    (forward-char -1)))

(defun fg/clear-last-search-char () 
  (setq fg/last-search-char nil))

(add-hook 'deactivate-mark-hook 'fg/clear-last-search-char)

(defun fg/repeat-fg-commands ()
  (interactive)
  (if fg/last-fg-command
	  (call-interactively fg/last-fg-command)))
