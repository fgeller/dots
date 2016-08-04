(use-package go-mode
  :ensure go-mode
  :commands go-mode
  :config
  (add-hook 'go-mode-hook 'golang-customizations)
  (use-package company-go :ensure company-go)
  (use-package go-eldoc :ensure go-eldoc))

(defun golang-customizations ()
  (subword-mode 1)
  (set (make-local-variable 'company-backends) '(company-go))
  (yas-minor-mode 1)
  (setq gofmt-command "goimports")
  (font-lock-mode 1)
  (setq scala-errors--error-re
        (rx bol
            (* space)
            (group (+ (not (any ":\n")))) ":"
            (group (+ (not (any ":\n")))) ":" (* space)
            (group (+ nonl))
            eol))
  (setq scala-errors--error-column-re nil)
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (eldoc-mode 1)
  (go-eldoc-setup)
  (flycheck-mode 1)
  (add-hook 'before-save-hook #'gofmt-before-save))

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
