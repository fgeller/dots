(install 'go-mode)

;; (setq lsp-go-gopls-server-args '("-logfile" "/home/fgeller/tmp/gopls.log" "-rpc.trace"))
;; (setq lsp-go-gopls-server-args '("serve" "--debug=localhost:6060"))

(defun fg/golang-customizations ()
  (defalias 'go-play-buffer nil)
  (defalias 'go-play-region nil)

  (require 'lsp-mode)

  (lsp-register-custom-settings
   '(("gopls.staticcheck" nil t)
     ("gopls.allowImplicitNetworkAccess" t t)
     ))
  (setq lsp-go-build-flags ["-tags=integration_test"])
  (let ((env (make-hash-table)))
    (puthash "GOPROXY" "proxy.golang.org,direct" env)
    (setq lsp-go-env env))

  (lsp-deferred)
  (lsp-diagnostics-modeline-mode)

  ;; (require 'squirrel)
  ;; (define-key squirrel-mode-map (kbd "I") 'squirrel-goto-next)
  ;; (define-key squirrel-mode-map (kbd "N") 'squirrel-goto-previous)
  ;; (define-key squirrel-mode-map (kbd "E") 'squirrel-goto-first-child)
  ;; (define-key squirrel-mode-map (kbd "O") 'squirrel-goto-parent)
  ;; (add-hook 'squirrel-motion-hook 'squirrel-mark-current-node nil t)
  ;; (squirrel-mode +1)
  
  (subword-mode 1)
  (yas-minor-mode)

  (setq tab-width 4)

  (setq display-fill-column-indicator-character ?\u2502)
  (display-fill-column-indicator-mode +1)

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
