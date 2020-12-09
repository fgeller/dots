(require '3w)

;; limit height of *Completions* buffer
(temp-buffer-resize-mode +1)
(setq temp-buffer-max-height 15)

(setq display-buffer-alist
      `(
	(,3w-side-window-rx (3w-display-as-side-window))
	(".*\\*Completions.*"
	 (display-buffer-reuse-window display-buffer-in-side-window)
	 (side . bottom)
	)))

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
