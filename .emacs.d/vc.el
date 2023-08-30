;; -*- lexical-binding: t -*-

(add-hook 'vc-annotate-mode-hook
		  (lambda () (setq show-trailing-whitespace nil)))

;; (setq vc-annotate-background-mode nil)

(after 'vc-annotate
  (define-key vc-annotate-mode-map (kbd "(") 'vc-annotate-toggle-annotation-visibility))

(setq vc-follow-symlinks t)
(setq vc-git-show-stash 3)

(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=short" "-C" "-C" rev "--" name)))

(setq git-commit-summary-max-length 72)

(setq diff-font-lock-prettify nil)
(setq diff-font-lock-syntax nil)

(install 'git-link)

(install 'diff-hl)

;; (global-diff-hl-mode)
;; (diff-hl-margin-mode +1)

(setq diff-hl-margin-symbols-alist
	  '((insert . " ")
		(delete . " ")
		(change . " ")
		(unknown . " ")
		(ignored . " ")))

(defun fg/vc-dir-project ()
  (interactive)
  (let ((root (fg/guess-project-directory)))
	(vc-dir root)))

(install 'agitate)
(add-hook 'diff-mode-hook #'agitate-diff-enable-outline-minor-mode)

(setq agitate-log-limit 2000)

(defun fg/customize-diff ()
  (define-key diff-mode-shared-map (kbd "TAB") 'outline-cycle)
  (define-key diff-mode-map (kbd "TAB") 'outline-cycle))

(add-hook 'diff-mode-hook 'fg/customize-diff)

(defun fg/project-name-p (str) 
  (and (not (member str (list "." "..")))
	   (not (string-prefix-p "." str))))

(defun fg/git-fetch ()
  (interactive)
  (process-lines vc-git-program "fetch"))

(defun fg/git-merge-main ()
  (interactive)
  (process-lines vc-git-program "merge" "--ff-only" "origin/main"))

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

(defun fg/pick-rev ()
  (interactive)
  (let ((default-directory (vc-root-dir))
		(rev (string-trim (fg/vc-git-local-branch-prompt))))
	(fg/checkout-rev rev)))

(defun fg/checkout-rev (rev &optional is-pr)
  (let* ((vc-root (vc-root-dir))
		 (default-directory vc-root))
	(when (yes-or-no-p (format "kill buffer's under %s?" vc-root))
	  (dolist (buf (buffer-list))
		(when (with-current-buffer buf
				(let ((dom-file (locate-dominating-file default-directory ".git")))
				  (and dom-file (file-equal-p dom-file vc-root))))
		  (kill-buffer buf))))
	(find-file vc-root)
	(when (lsp-workspaces) (lsp-shutdown-workspace))
	(if is-pr
		(process-lines "gh" "pr" "checkout" rev)
	  (vc-retrieve-tag vc-root rev))
	(fg/branch-overview vc-root)))

(defun fg/branch-overview (&optional dir)
  (interactive)
  (let ((default-directory (or dir (vc-root-dir))))
	(find-file default-directory)
	(vc-diff-mergebase default-directory "origin/main" "HEAD")
	(outline-cycle-buffer)
	(font-lock-mode 1)
	(delete-other-windows)
	(3w-split-2-1)
	(other-window 1)
	(vc-log-mergebase default-directory "origin/main" "HEAD")
	(other-window 1)))

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
									  "--limit" "50"
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
		 (pr (gethash (fg/number-prefix selected) prs))
		 )
	(if open-browser
		(browse-url (gethash "url" pr))
	  (fg/checkout-rev (number-to-string (gethash "number" pr)) 'is-pr))
	))

(defun fg/number-prefix (s)
  (if (string-match "^\\([0123456789]+\\)" s)
	  (string-to-number (match-string 1 s))
	(message "failed to string-match number prefix in %S" s)))

(defun fg/new-branch ()
  (interactive)
  (let ((vc-root (vc-root-dir)))
	(fg/checkout-rev "main")
	(fg/git-fetch)
	(fg/git-merge-main)
	(vc-create-branch vc-root (read-string "branch name: "))))
