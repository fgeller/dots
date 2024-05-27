(require '3w)

;; limit height of *Completions* buffer
(temp-buffer-resize-mode +1)
(setq temp-buffer-max-height 15)

(setq display-buffer-base-action '(3w-display-buffer-in-other-window))

(setq 3w-side-window-exceptions-rx
	  (rx (or
		   "*Annotate "
		   "*vc-dir*"
		   "*Org Src"
		   )))

(setq display-buffer-alist
      `(
		(,3w-side-window-rx (3w-display-as-side-window))
		(".*\\*Completions.*"
		 (display-buffer-reuse-window display-buffer-in-side-window)
		 (side . bottom)
		 )))

(defun fg/store-window-layout () 
  (interactive)
  (setq fg/last-window-layout (current-window-configuration)))

(defun fg/restore-window-layout ()
  (interactive)
  (set-window-configuration fg/last-window-layout))

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; restore win layout after quitting ediff
;; https://emacs.stackexchange.com/a/17089
(defvar fg/ediff-last-window-layout nil)

;; in case we need to reload some files, store the original layout - not the
;; reload confirmation layout
(defun fg/ediff-wrapper ()
  (interactive)
  (fg/store-pre-ediff-window-layout)
  (call-interactively 'ediff))

(defun fg/vc-ediff-wrapper ()
  (interactive)
  (fg/store-pre-ediff-window-layout)
  (call-interactively 'vc-ediff))

(defun fg/store-pre-ediff-window-layout ()
  (message "storing ediff config")
  (setq fg/ediff-last-window-layout (current-window-configuration)))

(defun fg/restore-pre-ediff-window-layout ()
  (message "RESTORING PRE EDIFF")
  (set-window-configuration fg/ediff-last-window-layout))

(add-hook 'ediff-quit-hook #'fg/restore-pre-ediff-window-layout)

