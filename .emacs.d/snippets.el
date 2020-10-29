(defconst snippet-point-marker "@@@"
  "Marker to identify location of point in snippet after expansion")

(defconst snippets
  '(
    ("ife" . "if err != nil {
	return @@@err
}")
    )
  "Alist of snippet keys to expanded version")


(defun try-snippet-expansion ()
  "Tries to identify snippet key at point using and replaces key by snippet if applicable"
  (interactive)
  (let* ((tt 'symbol)
	 (tap (thing-at-point tt))
	 (sa (assoc tap snippets)))
    (if (not sa) (message "found not snippet for [%s]" tap)
      (let* ((bds (bounds-of-thing-at-point tt)))
	(kill-region (car bds) (cdr bds))
	(insert (cdr sa))
	(let* ((sp (point))
	       (lm (- sp (length (cdr sa)))))
	  (indent-region lm sp)
	  ;; go to marker and cleanup if applicable
	  (search-backward snippet-point-marker lm t)
	  (unless (eq sp (point))
	    (delete-forward-char (length snippet-point-marker))))))))

(define-key global-map (kbd "C-'") 'try-snippet-expansion)
