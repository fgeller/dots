(install 'go-mode)

(defun golang-customizations ()
  (defalias 'go-play-buffer nil)
  (defalias 'go-play-region nil)

  (lsp-deferred)

  (lsp-diagnostics-modeline-mode)
  (subword-mode 1)

  (setq tab-width 4)

  (define-key go-mode-map (kbd "C-c C-c") 'go-run-all-tests)
  (define-key go-mode-map (kbd "C-c b") 'go-make-build)
  (define-key go-mode-map (kbd "C-c C-m") 'go-run-this-test)
  (define-key go-mode-map (kbd "C-c C-v") 'go-build-this)
  (define-key go-mode-map (kbd "C-c C-l") 'go-tests-toggle-truncate-lines)
  (define-key go-mode-map (kbd "C-c C-t") 'go-goto-first-error)
  (define-key go-mode-map (kbd "C-c C-n") 'go-goto-next-error)
  (define-key go-mode-map (kbd "C-c C-p") 'go-goto-previous-error)
  (define-key go-mode-map (kbd "C-c C-e") 'go-play)

  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook 'golang-customizations)

(defun go-tests-buffer-name ()
  (format "*go-test[%s in %s]*"
          (file-name-nondirectory (directory-file-name default-directory))
          (file-name-directory (directory-file-name default-directory))))

(defun go-tests-toggle-truncate-lines ()
  (interactive)
  (let ((buf (get-buffer (go-tests-buffer-name))))
    (when buf (with-current-buffer buf (toggle-truncate-lines)))))


(defun go-run-this-test ()
  (interactive)
  (go-run-tests (format "go test -i && go test -v -run %s"
                        (save-excursion
                          (re-search-backward "func \\(Test.+\\)(" (point-min))
                          (match-string 1)))))

(defun go-build-this ()
  (interactive)
  (go-run-tests (format "go build -i .")))

(defun go-run-all-tests ()
  (interactive)
  (go-run-tests "go test -i && go test -v"))

(defun go-run-tests (cmd)
  (let* ((dir (file-name-nondirectory (directory-file-name default-directory)))
         (buf (go-tests-buffer-name)))
    (if (get-buffer buf) (with-current-buffer buf (compile cmd))
      (compile cmd)
      (with-current-buffer "*compilation*" (rename-buffer buf)))))

(defun go-make-build ()
  (interactive)
  (let* ((dir (file-name-nondirectory (directory-file-name default-directory)))
         (buf (go-tests-buffer-name)))
    (if (get-buffer buf) (with-current-buffer buf (compile "make build"))
      (compile "make build")
      (with-current-buffer "*compilation*" (rename-buffer buf)))))

(defmacro go-goto-error (&rest body)
  `(let ((buf (go-tests-buffer-name)))
     (when (get-buffer buf)
       (with-current-buffer buf
         ,@body
         (compile-goto-error)))))

(defun go-goto-first-error ()
  (interactive)
  (go-goto-error (goto-char (point-min)) (compilation-next-error 1)))

(defun go-goto-next-error ()
  (interactive)
  (go-goto-error (compilation-next-error 1)))

(defun go-goto-previous-error ()
  (interactive)
  (go-goto-error (compilation-previous-error 1)))

(defun go-ignore-all-tests ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "func Test\\([^(]+\\)(" "func IgnoreTest\\1(" nil (point-min) (point-max))))

(defun go-enable-all-tests ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "func IgnoreTest\\([^(]+\\)(" "func Test\\1(" nil (point-min) (point-max))))

(defun go-play ()
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
