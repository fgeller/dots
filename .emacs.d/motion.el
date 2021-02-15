(defun fg/save-point ()
  (interactive)
  (point-to-register ?p))

(defun fg/restore-point ()
  (interactive)
  (jump-to-register ?p))

(install 'avy)
(setq avy-all-windows nil
      avy-keys '(?a ?s ?h ?g ?y ?t ?n ?e ?o ?i ?' ?u ?p ?d ?r ?c ?k)
      avy-timeout-seconds 0.2)

(defun scroll-down-half-page ()
  (interactive)
  (scroll-down-command (/ (window-height) 4))
  (pulse-momentary-highlight-one-line (point)))

(defun scroll-up-half-page ()
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

(defun move-to-next-symbol-occurrence ()
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

(defun move-to-previous-symbol-occurrence ()
  (interactive)
  (beginning-of-symbol)
  (let ((thing (thing-at-point 'symbol)))
    (setq isearch-string thing)
    (search-backward-regexp (concat "\\_<" (regexp-quote thing) "\\_>"))
    (save-excursion
      (forward-symbol 1)
      (push-mark (point)))))

(defun move-to-next-word-occurrence ()
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

(defun move-to-previous-word-occurrence ()
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

