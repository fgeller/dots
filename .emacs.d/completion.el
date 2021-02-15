(install 'company 'require)

(setq
 company-backends '(company-capf company-files company-elisp company-dabbrev)
 company-minimum-prefix-length 1
 company-idle-delay .1
 company-tooltip-idle-delay 1.0
 company-tooltip-align-annotations t)

(after 'company
  (define-key company-mode-map (kbd "C-n") 'company-select-next)
  (define-key company-mode-map (kbd "C-p") 'company-select-previous))

(global-company-mode 1)

;; based on https://emacs.stackexchange.com/a/13508
(defun fg/substrings-completion (str table predicate point &optional all-p)
  (let* ((before-point (substring str 0 point))
	 (after-point (substring str point))
	 (boundaries (completion-boundaries before-point table predicate after-point))
         (prefix (substring before-point 0 (car boundaries)))
         (infix (concat (substring before-point (car boundaries))
			(substring after-point 0 (cdr boundaries))))
         (suffix (substring after-point (cdr boundaries)))
	 (rxs (split-string infix " " t))
         (candidates (cl-remove-if-not
		      (lambda (c) (cl-every (lambda (r) (string-match-p r c))
					    rxs))
                      (all-completions prefix table predicate))))
    (cond
     (all-p
      (when candidates
        (setcdr (last candidates) (length prefix))
        candidates))

     ((and (= (length candidates) 1)
           (equal infix (car candidates)))
      t)

     ((= (length candidates) 1)
      (when (and (> (length (car candidates)) 0)
                 (> (length suffix) 0)
                 (char-equal (aref (car candidates)
                                   (1- (length (car candidates))))
                             (aref suffix 0)))
        (setq suffix (substring suffix 1)))
      (cons (concat prefix (car candidates) suffix)
            (length (concat prefix (car candidates)))))
     
     (t (cons str point)))))

(defun fg/substrings-try-completion (string table predicate point)
  (fg/substrings-completion string table predicate point))

(defun fg/substrings-all-completions (string table predicate point)
  (fg/substrings-completion string table predicate point 'all))

(add-to-list 'completion-styles-alist
             '(
	       substrings
	       fg/substrings-try-completion
	       fg/substrings-all-completions
	       "Splits query by spaces and matches each as a substring/rx against candidates. Never completes string, only filters matches."
	       ))

(setq completion-styles '(substrings))
(setq completion-ignore-case t)
(setq completion-show-help nil)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)

(icomplete-mode +1)
(setq icomplete-separator "  ")
(setq icomplete-prospects-height 1)
(setq icomplete-show-matches-on-no-input t)

(dolist (map (list minibuffer-local-completion-map
		   minibuffer-inactive-mode-map
		   icomplete-minibuffer-map))
  (define-key map (kbd "C-j") 'minibuffer-exit)
  (define-key map (kbd "RET") 'minibuffer-force-complete-and-exit)
  (define-key map (kbd "<return>") 'minibuffer-force-complete-and-exit)
  (define-key map (kbd "C-n") 'icomplete-forward-completions)
  (define-key map (kbd "C-p") 'icomplete-backward-completions)
  (define-key map (kbd "C-.") 'fg/zap-back-till-/))

(defun fg/zap-back-till-/ ()
  (interactive)
  (zap-up-to-char -1 ?/))

(defun switch-to-minibuffer ()
  (interactive)
  (let* ((amb (active-minibuffer-window)))
    (when amb (select-window amb))))

(define-key completion-list-mode-map (kbd "C-v") 'switch-to-minibuffer)

;; M-x

(defun m-x-annotation (c)
  (let* ((ks (where-is-internal (intern c)))
	 (sk (when ks (key-description (car ks)))))
    (if (and ks
	     (not (string-match-p "<menu" sk)))
	(concat "   " sk))))

(defun m-x-with-bindings ()
  (interactive)
  (let ((completion-extra-properties (list :annotation-function #'m-x-annotation)))
    (call-interactively 'execute-extended-command)))

;; JUMP

(defun buffer-name-candidates ()
  (mapcar (lambda (bn) (cons bn 'switch-to-buffer))
	  (cl-remove-if (lambda (bn) (string-match " \\*.+\\*"  bn)) ; " *Minibuf..." or " *Echo ..."
			(mapcar 'buffer-name (buffer-list)))))

(defun git-ls-files-candidates ()
  (let ((grt (locate-dominating-file default-directory ".git")))
    (when grt
      (let* ((default-directory grt)
	     (cmd (format "git ls-files --full-name"))
	     (files (split-string (shell-command-to-string cmd) "\n" t))
	     (action `(lambda (f) (find-file (file-name f ,grt)))))
	(mapcar (lambda (f) (cons f action)) files)))))

(defun recentf-candidates ()
  (recentf-mode)
  (mapcar (lambda (f) (cons f 'find-file)) recentf-list))

(defun jump-candidates ()
  (recentf-mode)
  (let* ((other-buf (list (cons (buffer-name (other-buffer)) 'switch-to-buffer)))
	 (buffers (buffer-name-candidates))
	 (git-files (git-ls-files-candidates))
	 (recentfs (recentf-candidates)))
    (seq-uniq (seq-concatenate 'list
			       other-buf
			       buffers
			       recentfs
			       git-files
			       ))))

(defun jump ()
  (interactive)
  (let* ((start-time (current-time))
	 (candidates (jump-candidates))
	 (default (car (car candidates)))
	 (elapsed (time-subtract (current-time) start-time))
	 (target (completing-read (format "jump (%s): " default) candidates nil nil nil nil (car (car candidates))))
	 (action (cdr (assoc target candidates))))
    ;; (message "jump>> elapsed %s" (format-time-string "%3N" elapsed))
    (funcall action target)))

(defun yank-from-kill-ring ()
  (interactive)
  (insert (completing-read (format "Insert [%s]: " (car kill-ring)) kill-ring nil t nil nil (car kill-ring))))
