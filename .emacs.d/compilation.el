(defun ansi-colorize-buffer ()
  (require 'ansi-color)
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'ansi-colorize-buffer)

(defface fg/leading-dash-face
  '((t (:foreground "red")))
  "Red face for lines that start with space–dash–space.")
(defface fg/leading-plus-face
  '((t (:foreground "#4CAF50")))
  "Green face for lines that start with space–dash–space.")

(defun fg/compilation ()
  (font-lock-mode 1)
  (setq show-trailing-whitespace nil)

 
  ;; change display of nobreakspace
  (setq-local glyphless-char-display
              (let ((table (make-char-table nil)))
                (set-char-table-parent table glyphless-char-display)
                (set-char-table-range table 160 'space) ; U+00A0
                table))
  (face-remap-add-relative 'nobreak-space 'default)

  (font-lock-mode 1)
  ;; Buffer‑local font‑lock rule
  (font-lock-add-keywords nil '(("^[ \t]+-[ \t].*" . 'fg/leading-dash-face)) t)
  (font-lock-add-keywords nil '(("^[ \t]+\\+[ \t].*" . 'fg/leading-plus-face)) t)
  (font-lock-flush))

 
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

