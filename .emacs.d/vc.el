;; -*- lexical-binding: t -*-

(add-hook 'vc-annotate-mode-hook
		  (lambda () (setq show-trailing-whitespace nil)))

;; (setq vc-annotate-background-mode nil)

(after 'vc-annotate
  (define-key vc-annotate-mode-map (kbd "(") 'vc-annotate-toggle-annotation-visibility))

(setq vc-follow-symlinks t)

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
  (shell-command "git fetch"))

(defun fg/git-merge-main ()
  (interactive)
  (shell-command "git merge --ff-only origin/main"))

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

(setq vc-git-show-stash 3)


(defun fg/pick-rev ()
  (interactive)
  (let ((vc-root (vc-root-dir))
		(rev (string-trim (fg/vc-git-revision-prompt))))
	(fg/checkout-rev rev)))

(defun fg/pick-pr (prefix)
  (interactive "P")
  (let ((vc-root (vc-root-dir))
		(rev (string-trim (fg/github-pull-request-prompt prefix))))
	(fg/checkout-rev rev)))

(defun fg/checkout-rev (rev)
  (when (yes-or-no-p (format "kill buffer's under %s?" vc-root))
	(dolist (buf (buffer-list))
	  (when (with-current-buffer buf
			  (let ((dom-file (locate-dominating-file default-directory ".git")))
				(and dom-file (file-equal-p dom-file vc-root))))
		(kill-buffer buf))))
  (find-file vc-root)
  (when (lsp-workspaces) (lsp-shutdown-workspace))
  (vc-retrieve-tag vc-root rev)
  (fg/branch-overview vc-root))

(defun fg/branch-overview (&optional dir)
  (interactive)
  (let ((default-directory (or dir (vc-root-dir))))
	(find-file default-directory)
	(vc-diff-mergebase dir "origin/main" "HEAD")
	(outline-cycle-buffer)
	(font-lock-mode 1)
	(delete-other-windows)
	(3w-split-2-1)
	(other-window 1)
	(vc-log-mergebase dir "origin/main" "HEAD")
	(other-window 1)))

(defun fg/vc-git-revision-prompt (&optional dir)
  (interactive)
  (let ((default-directory (or dir (vc-root-dir))))
    (completing-read
     "Select revision: "
     (process-lines vc-git-program "branch" "--all" "--format" "%(refname:short)")
     nil
	 t)))

(defun fg/github-pull-request-prompt (prefix &optional dir)
  (interactive "P")
  (let* ((default-directory (or dir (vc-root-dir)))
		 (selected (completing-read
					"Select pull request: "
					(if prefix
						(process-lines "gh" "pr" "list" "--json" "author,headRefName,title" "--template" "{{range .}}{{tablerow .headRefName .author.login .title}}{{end}}")
					  (process-lines "gh" "pr" "list" "-S" "review-requested:fgeller" "--json" "author,headRefName,title" "--template" "{{range .}}{{tablerow .headRefName .author.login .title}}{{end}}"))
					nil 
					t)))
	(string-match "^\\([^ ]+\\) .+" selected)
	(format "origin/%s" (match-string 1 selected))))

;; http://www.google.com

(defun fg/github-open-pull-request-review-requested-prompt (&optional dir)
  (interactive)
  (let* ((default-directory (or dir (vc-root-dir)))
		 (selected (completing-read
					"Select pull request: "
					(process-lines "gh" "pr" "list" "-S" "review-requested:fgeller" "--json" "author,title,url" "--template" "{{range .}}{{.title}} - {{.author.login}} | {{.url}}\n{{end}}")
					nil 
					t)))
	(string-match "^.+\\(https://github.com.+\\)" selected)
	(browse-url (match-string 1 selected))))


(defun fg/github-open-pull-request-all-prompt (&optional dir)
  (interactive)
  (let* ((default-directory (or dir (vc-root-dir)))
		 (keyword (read-string "keyword: "))
		 (url-prop-name 'gh-pr-url)
		 (stdout-lines (process-lines "gh" "pr" "list"
									  "-S" keyword 
									  "--state" "all"
									  "--limit" "50"
									  "--json" "author,title,url,createdAt,number"
									  "--jq" ".[]"))
		 (prs (let* ((result (make-hash-table)))
				(mapc (lambda (jo) (puthash (gethash "number" jo) jo result))
					  (mapcar 'json-parse-string stdout-lines))
				result))
		 (candidates (let* ((result))
					   (maphash (lambda (num jo)
								  (message "JO: %S" jo)
								  (setq result
										(let* ((author-name (gethash "name" (gethash "author" jo)))
											   (author-login (gethash "login" (gethash "author" jo)))
											   (author (if (string-equal "" author-name)
														   author-login
														 (downcase (car (string-split author-name " ")))))
											   (title (gethash "title" jo))
											   (cand (format "%s: %s - %s" num title author)))
										  (add-face-text-property (- (length cand) (length author))
																  (length cand)
																  '(:foreground "gray") 
																  nil 
																  cand)
										  (cons cand result))))
								prs)
					   result))
		 (selected (completing-read "Select pull request: " candidates nil t))
		 (pr-number (fg/number-prefix selected))
		 (pr (gethash pr-number prs))
		 )
	(browse-url (gethash "url" pr))
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
