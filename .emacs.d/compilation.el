
(require 'ansi-color)
(defun ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'ansi-colorize-buffer)

(defun compilation-customizations ()
  (font-lock-mode 1)
  (setq show-trailing-whitespace nil))

(add-hook 'compilation-mode-hook 'compilation-customizations)

(setq compilation-scroll-output 'first-error)
(setq compilation-always-kill t)

