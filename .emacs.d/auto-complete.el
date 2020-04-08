(install 'company 'require)

(setq
 company-backends '(company-capf company-files company-elisp company-dabbrev)
 company-idle-delay .75
 company-tooltip-idle-delay .75
 company-tooltip-align-annotations t)

(after 'company
  (define-key company-mode-map (kbd "C-n") 'company-select-next)
  (define-key company-mode-map (kbd "C-p") 'company-select-previous))

(global-company-mode 1)
