(install 'avy)
(setq avy-all-windows nil
      avy-background t
      avy-style 'de-bruijn
      avy-keys '(?a ?s ?h ?t ?n ?e ?o ?i)
      ;; avy-goto-word-0-regexp "\\(\\b\\sw\\|[(){}]\\)"
      avy-goto-word-0-regexp "\\b\\sw"
      avy-timeout-seconds 0.2)

(defun fg/back-node-or-word ()
  (interactive)
  (if squirrel-mode
	  (call-interactively 'squirrel-goto-previous)
	(call-interactively 'backward-word)))

(defun fg/forward-node-or-word ()
  (interactive)
  (if squirrel-mode
	  (call-interactively 'squirrel-goto-next)
	(call-interactively 'forward-word)))

(defun fg/down-node-or-scroll ()
  (interactive)
  (if squirrel-mode
	  (call-interactively 'squirrel-goto-first-child)
	(call-interactively 'fg/scroll-up-half-page)))

(defun fg/up-node-or-scroll ()
  (interactive)
  (if squirrel-mode
	  (call-interactively 'squirrel-goto-parent)
	(call-interactively 'fg/scroll-down-half-page)))

(defun fg/jump-to-char ()
  (interactive)
  (xref-push-marker-stack)
  (avy-goto-char-timer))
   
(defun fg/beginning-of-buffer ()
  (interactive)
  (xref-push-marker-stack)
  (beginning-of-buffer))

(defun fg/end-of-buffer ()
  (interactive)
  (xref-push-marker-stack)
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

;; https://www.emacswiki.org/emacs/NavigatingParentheses#h5o-2
(require 'xref)

(defun fg/jump-to-matching-paren ()
  (interactive)
  (cond ((looking-at "\\s(") (forward-sexp 1))
        ((looking-back "\\s)" 1) (backward-sexp 1))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp 1))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp 1))))

(defun fg/consult-goto-line ()
  (interactive)
  (xref-push-marker-stack)
  (consult-goto-line))

(defun fg/consult-line ()
  (interactive)
  (xref-push-marker-stack)
  (consult-line))

(defun fg/grep ()
  (interactive)
  (xref-push-marker-stack)
  (consult-git-grep))

(defun fg/jump ()
  (interactive)
  (xref-push-marker-stack)
  (consult-buffer))

(defun fg/imenu ()
  (interactive)
  (xref-push-marker-stack)
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
  (xref-push-marker-stack)
  (scroll-down-command (/ (window-height) 4))
  (pulse-momentary-highlight-one-line (point)))

(defun fg/scroll-up-half-page ()
  (interactive)
  (xref-push-marker-stack)
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
  (xref-push-marker-stack)
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
  (xref-push-marker-stack)
  (beginning-of-symbol)
  (let ((thing (thing-at-point 'symbol)))
    (setq isearch-string thing)
    (search-backward-regexp (concat "\\_<" (regexp-quote thing) "\\_>"))
    (save-excursion
      (forward-symbol 1)
      (push-mark (point)))))

(defun fg/move-to-next-word-occurrence ()
  (interactive)
  (xref-push-marker-stack)
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
  (xref-push-marker-stack)
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

