(require 'mhtml-mode)

(defun fg/mhtml-customizations ()
  (yas-minor-mode)
  (indent-tabs-mode t)
  (setq tab-width 2)
  (setq css-indent-offset 2))

(add-hook 'mhtml-mode-hook 'fg/mhtml-customizations)
