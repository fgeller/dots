(require-package 'go-mode)
(add-hook 'go-mode-hook 'golang-customizations)
(require-package 'go-rename)
(require-package 'company-go)
(require-package 'go-guru)
(require-package 'go-eldoc)

(defun golang-customizations ()
  (setq company-go-show-annotation 1)
  (setq gofmt-command "goimports")
  (set (make-local-variable 'company-backends) '(company-go))

  (subword-mode 1)
  (yas-minor-mode 1)
  (font-lock-mode 1)
  (eldoc-mode 1)
  (flycheck-mode 1)

  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "C-c C-r") 'go-rename)
  (define-key go-mode-map (kbd "C-c C-c") 'go-compile-tests)
  (define-key go-mode-map (kbd "C-c C-m") 'go-compile-this-test)
  (define-key go-mode-map (kbd "C-c C-t") 'go-goto-first-error)
  (define-key go-mode-map (kbd "C-c C-n") 'go-goto-next-error)
  (define-key go-mode-map (kbd "C-c C-p") 'go-goto-previous-error)

  (go-eldoc-setup)

  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'after-save-hook #'go-after-save-run-tests))

(require-package 'flycheck)
(require 'flycheck)
(flycheck-define-checker go-unused
  ""
  :command ("unused" ".")
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes go-mode
  :predicate flycheck-buffer-saved-p)
(flycheck-define-checker go-gosimple
  ""
  :command ("gosimple" ".")
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes go-mode
  :predicate flycheck-buffer-saved-p)

(add-to-list 'flycheck-checkers 'go-gosimple)
(add-to-list 'flycheck-checkers 'go-unused)
(flycheck-add-next-checker 'go-test 'go-gosimple 'append)
(flycheck-add-next-checker 'go-test 'go-unused 'append)

(defun go-tests-buffer-name ()
  (format "*go-test[%s]*"
	  (file-name-nondirectory (directory-file-name default-directory))))

(defun go-after-save-run-tests ()
  (interactive)
  (let* ((buf (go-tests-buffer-name)))
    (when (get-buffer buf)
      (with-current-buffer buf
	(recompile)))))

(defun go-compile-this-test ()
  (interactive)
  (let* ((dir (file-name-nondirectory (directory-file-name default-directory)))
	 (comp-buf (format "*go-test[%s]*" dir))
	 (test-name (save-excursion
		      (re-search-backward "func \\(Test.+\\)(" (point-min) )
		      (match-string 1)))
	 (compile-command (format "go test -i && go test -v -run %s" test-name)))
    (message "cmd %s" compile-command)
    (if (get-buffer comp-buf)
	(with-current-buffer comp-buf
	  (compile compile-command))
      (compile compile-command)
      (with-current-buffer "*compilation*"
	(rename-buffer comp-buf)))))

(defun go-compile-tests ()
  (interactive)
  (let* ((dir (file-name-nondirectory (directory-file-name default-directory)))
	 (comp-buf (format "*go-test[%s]*" dir))
	 (compile-command "go test -i && go test -v"))
    (if (get-buffer comp-buf) (with-current-buffer comp-buf (compile compile-command))
      (compile compile-command)
      (with-current-buffer "*compilation*"
	(rename-buffer comp-buf)))))

(defmacro go-with-test-buffer (&rest body)
  `(let ((buf (go-tests-buffer-name)))
     (when (get-buffer buf)
       (with-current-buffer buf
	 ,@body))))

(defun go-goto-first-error ()
  (interactive)
  (go-with-test-buffer
   (goto-char (point-min))
   (compilation-next-error 1)
   (compile-goto-error)))

(defun go-goto-next-error ()
  (interactive)
  (go-with-test-buffer
   (compilation-next-error 1)
   (compile-goto-error)))

(defun go-goto-previous-error ()
  (interactive)
  (go-with-test-buffer
   (compilation-previous-error 1)
   (compile-goto-error)))

(defun go-ignore-all-tests ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp
     "func Test\\([^(]+\\)("
     "func IgnoreTest\\1("
     nil
     (point-min)
     (point-max))))

(defun go-enable-all-tests ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp
     "func IgnoreTest\\([^(]+\\)("
     "func Test\\1("
     nil
     (point-min)
     (point-max))))

(defun go-play ()
  (interactive)
  (let* ((temporary-file-directory (expand-file-name "tmp/" (getenv "GOPATH")))
	 (tf
	  (progn
	    (make-directory temporary-file-directory t)
	    (make-temp-file "go-play" nil ".go"))))
    (find-file tf)
    (insert "package main

import (
	\"fmt\"
)

func main() {
	fmt.Printf(\"\")
}")
    (goto-char 61)
    (go-mode)
    (define-key
      (current-local-map)
      (kbd "C-c C-k")
      (lambda () (interactive)
	(save-buffer)
	(delete-file (buffer-file-name))
	(kill-buffer)))
    (define-key
      (current-local-map)
      (kbd "C-c C-c")
      (lambda () (interactive)
	(save-buffer)
	(compile (format "go run %s" (buffer-file-name)))))))
