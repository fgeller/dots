(install 'go-mode)

(defun fg/golang-customizations ()
  (defalias 'go-play-buffer nil)
  (defalias 'go-play-region nil)

  (lsp-deferred)

  (lsp-diagnostics-modeline-mode)
  (subword-mode 1)

  (setq tab-width 4)

  (evil-collection-define-key 'normal 'go-mode-map
    "gd" 'xref-find-definitions
    (kbd "C-t") 'xref-pop-marker-stack)

  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook 'fg/golang-customizations)

(defun fg/go-compilation-buffer-name ()
  (format "*go-compilation[%s in %s]*"
          (file-name-nondirectory (directory-file-name default-directory))
          (file-name-directory (directory-file-name default-directory))))

(defun fg/go-compilation-toggle-truncate-lines ()
  (interactive)
  (let ((buf (get-buffer (fg/go-compilation-buffer-name))))
    (when buf (with-current-buffer buf (toggle-truncate-lines)))))


(defun fg/go-run-this-test ()
  (interactive)
  (fg/go-run (format "go test -v -vet=all -run %s"
                        (save-excursion
                          (re-search-backward "func \\(Test.+\\)(" (point-min))
                          (match-string 1)))))

(defun fg/go-build-this ()
  (interactive)
  (fg/go-run (format "go build .")))

(defun fg/go-run-this ()
  (interactive)
  (fg/go-run "go run *.go"))

(defun fg/go-run-all-tests ()
  (interactive)
  (fg/go-run "go test -v -vet=all"))

(defun fg/go-run (cmd)
  (let* ((dir (file-name-nondirectory (directory-file-name default-directory)))
         (buf (fg/go-compilation-buffer-name)))
    (if (get-buffer buf) (with-current-buffer buf (compile cmd))
      (compile cmd)
      (with-current-buffer "*compilation*" (rename-buffer buf)))))

(defun fg/go-make-build ()
  (interactive)
  (let* ((dir (file-name-nondirectory (directory-file-name default-directory)))
         (buf (fg/go-compilation-buffer-name)))
    (if (get-buffer buf) (with-current-buffer buf (compile "make build"))
      (compile "make build")
      (with-current-buffer "*compilation*" (rename-buffer buf)))))

(defun fg/go-make-tests ()
  (interactive)
  (let* ((dir (file-name-nondirectory (directory-file-name default-directory)))
         (buf (fg/go-compilation-buffer-name)))
    (if (get-buffer buf) (with-current-buffer buf (compile "make test"))
      (compile "make test")
      (with-current-buffer "*compilation*" (rename-buffer buf)))))


(defmacro fg/go-goto-error (&rest body)
  `(let ((buf (fg/go-compilation-buffer-name)))
     (when (get-buffer buf)
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

(defun fg/go-play ()
  (interactive)
  (let* ((temporary-file-directory (expand-file-name "tmp/" (getenv "GOPATH")))
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
    (define-key (current-local-map) (kbd "C-c C-k")
      (lambda () (interactive) (save-buffer) (delete-file buffer-file-name) (kill-buffer)))
    (define-key (current-local-map) (kbd "C-c C-c")
      (lambda () (interactive) (save-buffer) (compile (format "go run %s" buffer-file-name))))))
