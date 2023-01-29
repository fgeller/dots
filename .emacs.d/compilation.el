(defun ansi-colorize-buffer ()
  (require 'ansi-color)
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'ansi-colorize-buffer)

(defun fg/compilation ()
  (font-lock-mode 1)
  (setq show-trailing-whitespace nil))

(add-hook 'compilation-mode-hook 'fg/compilation)

(setq compilation-scroll-output 'first-error)
(setq compilation-always-kill t)
(setq compilation-ask-about-save nil)

(defun fg/guess-project-directory ()
  (let* ((dd default-directory)
		 (git (locate-dominating-file "." ".git"))
		 (mk (locate-dominating-file "." "makefile")))
	(or git
		mk
		dd)))

(defun fg/project-compile ()
  (interactive)
  (let* ((default-directory (fg/guess-project-directory)))
    (call-interactively 'compile)))

