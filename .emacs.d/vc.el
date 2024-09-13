;; -*- lexical-binding: t -*-

(use-package vc
  :commands (vc-dir vc-root-diff)
  :defer 1
  :config
  (setq vc-follow-symlinks t)
  (setq vc-git-show-stash 3))

(use-package vc-annotate 
  :commands vc-annotate
  :config 
  (define-key vc-annotate-mode-map (kbd "(") 'vc-annotate-toggle-annotation-visibility)
  
  (defun vc-git-annotate-command (file buf &optional rev)
	(let ((name (file-relative-name file)))
      (vc-git-command buf 'async nil "blame" "--date=short" "-C" "-C" rev "--" name)))
  
  (defun fg/vc-annotate-mode-hook ()
	(setq show-trailing-whitespace nil))
  
  (add-hook 'vc-annotate-mode-hook 'fg/vc-annotate-mode-hook))

(use-package git-link
  :ensure t
  :commands (git-link)
  :config
  (setq git-link-default-branch "main"))

(use-package diff-hl
  :ensure t
  :commands (diff-hl-next-hunk diff-hl-previous-hunk diff-hl-revert-hunk global-diff-hl-mode diff-hl-margin-mode)
  :defer 5
  :config
  (setq diff-hl-margin-symbols-alist
		'((insert . " ")
		  (delete . " ")
		  (change . " ")
		  (unknown . " ")
		  (ignored . " ")))
  :init
  (global-diff-hl-mode)
  (diff-hl-margin-mode +1))

(use-package agitate
  :ensure t
  :commands (agitate-diff-enable-outline-minor-mode)
  :config
  (setq agitate-log-limit 2000))

(defun fg/vc-dir-project ()
  (interactive)
  (let ((root (fg/guess-project-directory)))
	(vc-dir root)))

(defun fg/diff-mode-hook () 
  (font-lock-mode 1)
  (setq diff-font-lock-prettify nil)
  (setq diff-font-lock-syntax nil)
  (agitate-diff-enable-outline-minor-mode)
  (define-key diff-mode-shared-map (kbd "TAB") 'outline-cycle)
  (define-key diff-mode-map (kbd "TAB") 'outline-cycle))

(add-hook 'diff-mode-hook #'fg/diff-mode-hook)

(defun fg/project-name-p (str) 
  (and (not (member str (list "." "..")))
	   (not (string-prefix-p "." str))))

(defun fg/git-fetch ()
  (interactive)
  (process-lines vc-git-program "fetch"))

(defun fg/git-checkout-main ()
  (interactive)
  (process-lines vc-git-program "checkout" "main"))

(defun fg/git-ff-main ()
  (interactive)
  (process-lines vc-git-program "merge" "--ff-only" "origin/main"))

(defun fg/git-merge-main ()
  (interactive)
  (process-lines vc-git-program "merge" "origin/main"))

(defun fg/visit-project ()
  (interactive)
  (let* ((github "~/src/github.com/")
		 org-path
		 proj-path
		 projs
		 selected-proj)
	(dolist (org (directory-files (expand-file-name github)))
	  (setq org-path (format "%s%s" github org))
	  (when (fg/project-name-p org)
		(dolist (proj (directory-files (expand-file-name org github)))
		  (setq proj-path (format "%s/%s" org-path proj))
		  (when (fg/project-name-p proj)
			(setq projs (cons proj-path projs))))))
	(setq selected-proj (completing-read "Select project: " projs nil t))
	(find-file selected-proj)
	(when (vc-responsible-backend selected-proj 'no-error)
	  (vc-dir selected-proj))
	(delete-other-windows)))

(defun fg/checkout-branch ()
  (interactive)
  (let ((default-directory (vc-root-dir))
		(rev (string-trim (fg/vc-git-local-branch-prompt))))
	(fg/checkout-rev rev)))

(defun fg/clear-project-buffers (&optional root)
  (interactive)
  (let ((root (or root 
				  (locate-dominating-file default-directory ".git")
				  (fg/guess-project-directory))))
	(when (yes-or-no-p (format "kill buffer's under %s?" root))
	  (mapc (lambda (buf)
			  (with-current-buffer buf
				(when (and default-directory
						   (let ((buf-dom-file (locate-dominating-file default-directory ".git")))
							 (and buf-dom-file
								  (file-equal-p buf-dom-file root))))
				  (kill-buffer))))
			(buffer-list)))
	(when (and lspce-mode
			   (yes-or-no-p (format "shutdown lsp?" root)))
	  (lspce-shutdown-server))
	(find-file root)
	))

(defun fg/checkout-rev (rev &optional is-pr)
  (let* ((vc-root (vc-root-dir))
		 (dirty-p (> (length (process-lines "git" "status" "--porcelain")) 0)))
	(if dirty-p 
		(progn 
		  (vc-root-diff nil)
		  (message "can't checkout rev %s as working directory is dirty" rev))
	  (fg/clear-project-buffers vc-root)
	  (if is-pr
		  (process-lines "gh" "pr" "checkout" rev)
		(vc-retrieve-tag vc-root rev))
	  (if (string-equal "main" rev)
		  (vc-dir vc-root)
		(fg/branch-overview vc-root)))))

(defun fg/branch-overview (&optional dir)
  (interactive)
  (let* ((vc-root (or dir 
					  (vc-root-dir) 
					  (fg/guess-project-directory)))
		 (default-directory vc-root))
	(find-file vc-root)
	(vc-diff-mergebase vc-root "origin/main" "HEAD")
	(outline-cycle-buffer)
	(font-lock-mode 1)
	(delete-other-windows)
	(3w-split-2-1)
	(other-window 1)
	(let ((default-directory vc-root)
		  (buf (get-buffer-create (format "*fg/diff --stat*"))))
	  (let ((inhibit-message t)) (shell-command "git diff origin/main...HEAD --stat" buf))
	  (vc-log-mergebase vc-root "origin/main" "HEAD")
	  (split-window-below)
	  (other-window 1)
	  (switch-to-buffer buf)
	  (3w-jump-1))))

(defun fg/vc-git-local-branch-prompt (&optional dir)
  (interactive)
  (let ((default-directory (or dir (vc-root-dir))))
    (completing-read
     "Select revision: "
     (process-lines vc-git-program "branch" "--all" "--format" "%(refname:short)")
     nil
	 t)))

(defun fg/github-open-pull-request (&optional dir)
  (interactive)
  (let* ((review-requested (yes-or-no-p "only review requested? "))
		 (open-state (yes-or-no-p "only open state? "))
		 (open-browser (yes-or-no-p "open in browser? "))
		 (keyword (read-string "search keyword: "))
		 (default-directory (or dir (vc-root-dir)))
		 (_ (let ((inhibit-message t)) (fg/git-fetch)))
		 (stdout-lines (process-lines "gh" "pr" "list"
									  "-S" (if review-requested 
											   (format "review-requested:fgeller %s" keyword)
											 keyword)
									  "--state" (if open-state "open" "all")
									  "--limit" "100"
									  "--json" "author,title,url,createdAt,headRefName,number"
									  "--jq" ".[]"))
		 (prs (let* ((result (make-hash-table)))
				(mapc (lambda (jo) (puthash (gethash "number" jo) jo result))
					  (mapcar 'json-parse-string stdout-lines))
				result))
		 (candidates (let* ((result))
					   (maphash (lambda (num jo)
								  (setq result
										(let* ((author-name (gethash "name" (gethash "author" jo)))
											   (author-login (gethash "login" (gethash "author" jo)))
											   (author (if (or (not author-name)
															   (string-equal "" (string-trim author-name)))
														   author-login
														 (downcase (car (string-split author-name " ")))))
											   (title (gethash "title" jo))
											   (cand (format "%s: %s\t%s" num title author)))
										  (add-face-text-property (- (length cand) (length author))
																  (length cand)
																  '(:foreground "gray") 
																  nil 
																  cand)
										  (cons cand result))))
								prs)
					   result))
		 (selected (completing-read "Select pull request: " candidates nil t))
		 (pr (gethash (fg/number-prefix selected) prs)))
	(if open-browser
		(browse-url (gethash "url" pr))
	  (fg/checkout-rev (number-to-string (gethash "number" pr)) 'is-pr))))

(defun fg/number-prefix (s)
  (if (string-match "^\\([0123456789]+\\)" s)
	  (string-to-number (match-string 1 s))
	(message "failed to string-match number prefix in %S" s)))

(defun fg/git (&rest args)
  (let ((cmd (format "%s %s" vc-git-program args)))
	(message cmd)
	(apply 'process-lines (cons vc-git-program args))))

(defun fg/new-branch ()
  (interactive)
  (let ((vc-root (vc-root-dir)))
	(fg/git "checkout" "main")
	(fg/git "fetch" "origin")
	(fg/git  "merge" "--ff-only" "origin/main")
	(vc-create-branch vc-root (read-string "branch name: "))
	(vc-dir vc-root)
))

(defun fg/create-branch ()
  (interactive)
  (fg/git "checkout" "-b" (read-string "branch name: " "fg/"))
  (vc-dir (vc-root-dir)))

(defun fg/vc-git-show ()
  (interactive)
  (let* ((cw (current-word))
		 (rev (read-string "rev: " (when (string-match-p "[0-9a-z]+" cw) cw)))
		 (buf (get-buffer-create (format "*fg/vc-git-show %s" rev)))
		 (inhibit-message t))
	(shell-command (format "%s show %s" vc-git-program rev) buf)
	(switch-to-buffer buf)
	(read-only-mode 1)
	(diff-mode)
	(font-lock-mode 1)))

(defun fg/gh-pr-view ()
  (interactive)
  (let* ((cw (current-word))
		 (pr-num (or (when (string-match-p "[0-9]+" cw) cw)
					 (read-string "pr: "))))
	(shell-command (format "%s pr view --web %s" 
						   (locate-file "gh" exec-path exec-suffixes) 
						   pr-num))))

(defun fg/gh-repo-view ()
  (interactive)
  (shell-command (format "%s repo view --web" 
						   (locate-file "gh" exec-path exec-suffixes))))

