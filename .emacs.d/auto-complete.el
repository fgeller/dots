(install 'company 'require)

(setq
 company-backends '(company-capf company-files company-elisp company-dabbrev)
 company-idle-delay .75
 company-tooltip-idle-delay .75
 company-tooltip-align-annotations t)

(after 'company
  (define-key company-mode-map (kbd "C-n") 'company-select-next)
  (define-key company-mode-map (kbd "C-p") 'company-select-previous))

(global-company-mode 1)

;; TODO: counsel-unicode
;; TODO: mark ring
;; TODO: counsel-yank-pop
;; TODO: counsel-find-file

(install 'orderless)
(setq completion-styles '(orderless))
(setq completion-ignore-case t)
(setq completion-show-help nil)

(install 'live-completions)
(live-completions-mode +1)
(setq live-completions-columns 'single)
(setq live-completions-sort-order 'cycle)

(dolist (map (list minibuffer-local-completion-map
		   minibuffer-inactive-mode-map
		   minibuffer-local-map))
  (define-key map (kbd "C-j") 'minibuffer-exit)
  (define-key map (kbd "RET") 'minibuffer-force-complete-and-exit)
  (define-key map (kbd "<return>") 'minibuffer-force-complete-and-exit)
  (define-key map (kbd "C-v") 'switch-to-completions)
  )

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
