(install 'company)
(global-company-mode 1)
(setq company-backends '(company-bbdb company-capf company-files company-elisp company-dabbrev))
(setq company-idle-delay .12)
(setq company-tooltip-idle-delay .12)
(setq company-tooltip-align-annotations t)
(define-key company-mode-map (kbd "C-n") 'company-select-next)
(define-key company-mode-map (kbd "C-p") 'company-select-previous)

