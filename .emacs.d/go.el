(setq-default which-func-format '(:propertize (" " which-func-current " ") face completions-common-part))

(use-package go-ts-mode
  :mode ("\\.go\\'" . go-ts-mode)
  :config 

  (defun fg/go-mode-hook ()
    (font-lock-mode 1)
    (eldoc-mode 1)
    (subword-mode 1)
    (yas-minor-mode)
    (setq tab-width 4)
    (eglot-ensure)
    ;; (lsp-mode)
    (setq apheleia-formatter 'gofumpt)
    (apheleia-mode +1)
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (which-function-mode)
    (hs-minor-mode 1)
    (remove-hook 'find-file-hook 'which-func-ff-hook)
    (add-hook 'find-file-hook 'fg/delayed-which-func-setup)
    (add-hook 'before-save-hook
	      'fg/organize-go-imports
	      nil t))
  
  (add-hook 'go-ts-mode-hook 'fg/go-mode-hook)
  
  (setq display-fill-column-indicator-character ?\u2502)
  (display-fill-column-indicator-mode +1))

(defvar fg/which-func-delay-timer nil)
(defun fg/delayed-which-func-setup ()
  (when fg/which-func-delay-timer (cancel-timer fg/which-func-delay-timer))
  (setq fg/which-func-delay-timer
        (run-with-idle-timer 1.0 nil
                             (lambda ()
                               (when (and (buffer-live-p (current-buffer))
                                         (eq major-mode 'go-ts-mode))
                                 (which-function-mode 1))))))



(defun fg/organize-go-imports ()
  (ignore-errors
    (call-interactively 'eglot-code-action-organize-imports)))


(defun fg/convert-go-stack-trace-file-names (orig-fun &rest args)
  (let* ((marker (car args))
         (fn (cadr args))
         (fn-path)
         (dir (caddr args))
         (fmts (cadddr args))
         (git-fs)
         (dd))
    (cond
     ;; Handle absolute paths: /full/path/to/file.go:line
     ((string-match "[ \t]*\\(/.*\\.go\\):" fn)
      (let ((abs-path (match-string 1 fn)))
        (if (file-exists-p abs-path)
            ;; File exists at absolute path, use it directly
            (apply orig-fun (list marker abs-path dir fmts))
          ;; File doesn't exist at absolute path, try to find it in git repo
          (setq dd (with-current-buffer (marker-buffer marker) 
                     (locate-dominating-file "." ".git")))
          (when dd
            (let* ((filename (file-name-nondirectory abs-path))
                   (default-directory dd))
              (setq git-fs
                    (cl-remove-if-not 
                     (lambda (git-fn) (string-suffix-p filename git-fn))
                     (process-lines "git" "ls-files" "--full-name")))
              (if git-fs
                  (apply orig-fun (list marker (format "%s%s" dd (car git-fs)) dir fmts))
                (apply orig-fun args)))))))
     
     ;; Handle relative paths: pkg.fn_sth.go:111 or fn_sth.go:222
     ((string-match "[ \t]*\\([^/.]+\\.\\)?\\(.+\\.go\\)" fn)
      (let* ((pkg (match-string 1 fn))
             (go-fn (match-string 2 fn)))
        (setq fn-path (if (and pkg (> (length pkg) 0))
                          (format "/%s/%s" (substring pkg 0 (1- (length pkg))) go-fn)
                        (format "/%s" go-fn)))
        (setq dd (with-current-buffer (marker-buffer marker) 
                   (locate-dominating-file "." ".git")))
        (when dd
          (setq git-fs
                (let ((default-directory dd))
                  (cl-remove-if-not 
                   (lambda (git-fn) (string-suffix-p fn-path git-fn))
                   (process-lines "git" "ls-files" "--full-name"))))
          (if git-fs
              (apply orig-fun (list marker (format "%s%s" dd (car git-fs)) dir fmts))
            (apply orig-fun args)))))
     
     ;; Default case: no match, use original function
     (t (apply orig-fun args)))))


;; monkey patch compilation helper to jump to error file.
;; stack trace doesn't always contain the absolute path, but pkg/fn.go
;; don't see another way to customize the file matching 🥷
(advice-add 'compilation-find-file-1 :around #'fg/convert-go-stack-trace-file-names)

(defun fg/go-compilation-buffer-name ()
  (let* ((proj-path (expand-file-name (fg/guess-project-directory)))
		 (proj-name (file-name-nondirectory (directory-file-name proj-path)))
		 (loc (substring (directory-file-name (expand-file-name default-directory))
						 (- (length proj-path) (length proj-name) 1))))
	(format "*go-compilation[%s]*" proj-name)))

(defun fg/go-compilation-buffer-p (buf)
  (string-prefix-p "*go-compilation[" (buffer-name buf)))

(defun fg/go-find-compilation-buffer ()
  (if (get-buffer (fg/go-compilation-buffer-name))
      (get-buffer (fg/go-compilation-buffer-name))
    (-first 'fg/go-compilation-buffer-p (buffer-list))))

(defun fg/go-recompile ()
  (interactive)
  (let* ((buf (fg/go-find-compilation-buffer)))
    (when buf
      (with-current-buffer buf (recompile)))))

(defun fg/kill-go-buffers ()
  (interactive)
  (mapc (lambda (b) (let* ((name (buffer-name b))
						   (mm (with-current-buffer b major-mode)))
					  (when (or (eq mm 'go-mode)
								(eq mm 'go-ts-mode)
								(string-prefix-p "*gopls" name)
								(string-prefix-p "*go-compilation" name))
						(kill-buffer b))))
		(buffer-list)))

(defun fg/go-run-this-test ()
  (interactive)
  (fg/go-run (format "test -v -vet=all -tags test_dbt,local_development -run %s"
                        (save-excursion
                          (re-search-backward "func \\(Test.+\\)(" (point-min))
                          (match-string 1)))))

(defun fg/go-build-this ()
  (interactive)
  (fg/go-run "build ."))

(defun fg/go-run-this ()
  (interactive)
  (fg/go-run "run *.go"))

(defun fg/go-run-all-tests ()
  (interactive)
  (fg/go-run "test -v -vet=all"))

(defun fg/go-run (cmd)
  (let* ((cmd (format "%s %s"
					  (locate-file "go" exec-path exec-suffixes)
					  cmd))
		 (current-dir default-directory)
		 (dir (file-name-nondirectory (directory-file-name default-directory)))
         (buf (fg/go-compilation-buffer-name)))
	(when (and (get-buffer buf)
			   (with-current-buffer buf (not (string= current-dir default-directory))))
	  (kill-buffer buf))
    (if (get-buffer buf) 
		(with-current-buffer buf
		  (compile cmd))
      (compile cmd)
      (with-current-buffer "*compilation*" 
		(rename-buffer buf)
		(toggle-truncate-lines -1)))))

(defun fg/go-make-tests ()
  (interactive)
  (let* ((dir (file-name-nondirectory (directory-file-name default-directory)))
         (buf (fg/go-compilation-buffer-name)))
    (if (get-buffer buf) (with-current-buffer buf (compile "make test"))
      (compile "make test")
      (with-current-buffer "*compilation*" (rename-buffer buf)))))

(defmacro fg/go-goto-error (&rest body)
  `(let ((buf (fg/go-find-compilation-buffer)))
     (when buf
       (with-current-buffer buf
         ,@body
         (compile-goto-error)))))

(defun fg/go-goto-first-error ()
  (interactive)
  (fg/go-goto-error (goto-char (point-min)) (compilation-next-error 1)))

(defun fg/go-goto-next-error ()
  (interactive)
  (fg/go-goto-error (compilation-next-error 1)))

(defun fg/go-goto-previous-error ()
  (interactive)
  (fg/go-goto-error (compilation-previous-error 1)))

(defun fg/go-ignore-all-tests ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "func Test\\([^(]+\\)(" "func IgnoreTest\\1(" nil (point-min) (point-max))))

(defun fg/go-enable-all-tests ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "func IgnoreTest\\([^(]+\\)(" "func Test\\1(" nil (point-min) (point-max))))

(defun fg/go-play (name)
  (interactive
   (list (read-string "Name: " (format-time-string "%Y-%m-%dT%H-%M") t)))
  (let* ((temporary-file-directory (expand-file-name (format "~/src/github.com/fgeller/go-playground/%s" name)))
         (temp-file (progn
                      (make-directory temporary-file-directory t)
                      (make-temp-file "go-play" nil ".go"))))
    (find-file temp-file)
    (insert "package main

import \"fmt\"

func main() {
	fmt.Printf(\"\")
}")
    (goto-char 55)
    (go-mode)
    (define-key (current-local-map) (kbd "C-c C-k") 'fg/go-play-kill)
    (define-key (current-local-map) (kbd "C-c C-c") 'fg/go-play-run)))

(defun fg/go-play-run ()
  (interactive)
  (save-buffer)
  (exec-path-from-shell-initialize)
  (compile (format "/opt/homebrew/bin/go run %s" buffer-file-name)))

(defun fg/go-play-kill ()
  (interactive)
  (save-buffer)
  (delete-file buffer-file-name)
  (kill-buffer))


(defun fg/explode-func-args ()
  (interactive)
  (let* ((start-pos (save-excursion 
					  (unless (looking-at-p "(") (search-backward "("))
					  (point)))
		 (end-pos (save-excursion 
					(goto-char start-pos)
					(fg/jump-to-matching-paren)
					(point))))
	(narrow-to-region start-pos end-pos)
	(beginning-of-buffer)
	(forward-char 1)
	(insert "\n")
	(while (search-forward "," nil t)
	  (insert "\n"))
	(end-of-buffer)
	(forward-char -1)
	(unless (looking-at-p ",") (insert ","))
	(insert "\n")
	(widen)))


(defun fg/collapse-func-args ()
  (interactive)
  (let* ((start-pos (save-excursion 
					  (search-backward "(")
					  (point)))
		 (end-pos (save-excursion 
					(goto-char start-pos)
					(fg/jump-to-matching-paren)
					(point))))
	(narrow-to-region start-pos end-pos)
	(beginning-of-buffer)
	(replace-regexp "\n" "" nil start-pos end-pos)
	(end-of-buffer)
	(search-backward ")")
	(skip-chars-backward " \t(,")
	(when (looking-at-p ",") (delete-char 1))
	(goto-char start-pos)
	(widen)
))
