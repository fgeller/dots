(defun ansi-colorize-buffer ()
  (require 'ansi-color)
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'ansi-colorize-buffer)

(defface fg/test-want-face '((t (:inherit diff-added))) "")
(defface fg/test-got-face '((t (:inherit diff-removed))) "")

(defun fg/compilation ()
  (font-lock-add-keywords nil '(("^[ \t]+-[ \t].*" . 'fg/test-want-face)) t)
  (font-lock-add-keywords nil '(("^[ \t]+\\+[ \t].*" . 'fg/test-got-face)) t)
  (font-lock-mode 1)
  ;; (font-lock-flush)

  (setq show-trailing-whitespace nil)

 
  ;; change display of nobreakspace
  (setq-local glyphless-char-display
              (let ((table (make-char-table nil)))
                (set-char-table-parent table glyphless-char-display)
                (set-char-table-range table 160 'space) ; U+00A0
                table))
  (face-remap-add-relative 'nobreak-space 'default)


  (setq-local compilation-error-regexp-alist-alist
              (cons '(fg/go-trace
                      "^[ \t]*Error Trace:[ \t]*\\([^:\n]+\\):\\([0-9]+\\)"
                      1 2)
                    compilation-error-regexp-alist-alist))
  (setq-local compilation-error-regexp-alist-alist
              (cons '(fg/go-diff
                      "^[ \t]+\\(.*?\\.go\\):\\([0-9]+\\):[ \t]+diff -want(left)"
                      1 2)
                    compilation-error-regexp-alist-alist))
  (setq-local compilation-error-regexp-alist
              (cons 'fg/go-trace compilation-error-regexp-alist))
  (setq-local compilation-error-regexp-alist
              (cons 'fg/go-diff compilation-error-regexp-alist))

  ;; re-parse buffer so RET immediately works
  ;; (when (fboundp 'compilation--flush-parse)
  ;;   (compilation--flush-parse (point-min) (point-max)))
)

 
(add-hook 'compilation-mode-hook 'fg/compilation)

(setq compilation-scroll-output nil)
(setq compilation-always-kill t)
(setq compilation-ask-about-save nil)
(setq compilation-max-output-line-length nil)

(defun fg/guess-project-directory ()
  (let* ((dd default-directory)
		 (git (locate-dominating-file "." ".git"))
		 (mk (locate-dominating-file "." "makefile"))
		 (jst (locate-dominating-file "." "Justfile")))
	(or jst
		mk
		git
		dd)))

(defun fg/project-compile ()
  (interactive)
  (let* ((default-directory (fg/guess-project-directory)))
    (call-interactively 'compile)))

