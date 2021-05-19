(install 'evil)
(evil-mode 1)
(require 'evil-workman-mode)
(evil-workman-global-mode 1)

;; tough to move from motion yneo back to home position
;; text objects and pretty cool
;; cursor handling f vs t needs some getting used to
;; TODO evil-surround

(defconst fg/leader-keymap (make-keymap))
(define-key fg/leader-keymap (kbd "xf") 'find-file)
(define-key fg/leader-keymap (kbd "xx") 'execute-extended-command)
(define-key fg/leader-keymap (kbd "xs") 'save-buffer)
(define-key fg/leader-keymap (kbd "xb") 'switch-to-buffer)
(define-key fg/leader-keymap (kbd "SPC") 'fg/jump)

(evil-define-key '(visual normal) 'global (kbd "SPC") fg/leader-keymap)

 ;; o -> l  (open line below/above)
 ;; j -> n
 ;; k -> e
 ;; h -> y

(install 'evil-surround)
(global-evil-surround-mode 1)

;; JUMP

(defun fg/buffer-name-candidates ()
  (mapcar (lambda (bn) (cons bn 'switch-to-buffer))
	  (cl-remove-if (lambda (bn) (string-match " \\*.+\\*"  bn)) ; " *Minibuf..." or " *Echo ..."
			(mapcar 'buffer-name (buffer-list)))))

(defun fg/git-ls-files-candidates ()
  (let ((grt (locate-dominating-file default-directory ".git")))
    (when grt
      (let* ((default-directory grt)
	     (cmd (format "git ls-files --full-name"))
	     (files (split-string (shell-command-to-string cmd) "\n" t))
	     (action `(lambda (f) (find-file (file-name f ,grt)))))
	(mapcar (lambda (f) (cons f action)) files)))))

(defun fg/recentf-candidates ()
  (recentf-mode)
  (mapcar (lambda (f) (cons f 'find-file)) recentf-list))

(defun fg/jump-candidates ()
  (recentf-mode)
  (let* ((other-buf (list (cons (buffer-name (other-buffer)) 'switch-to-buffer)))
	 (buffers (fg/buffer-name-candidates))
	 (git-files (fg/git-ls-files-candidates))
	 (recentfs (fg/recentf-candidates)))
    (seq-uniq (seq-concatenate 'list
			       other-buf
			       buffers
			       recentfs
			       git-files
			       ))))

(defun fg/jump ()
  (interactive)
  (let* ((candidates (fg/jump-candidates))
	 (default (car (car candidates)))
	 (target (completing-read (format "jump (%s): " default) candidates nil nil nil nil (car (car candidates))))
	 (action (cdr (assoc target candidates))))
    (funcall action target)))

