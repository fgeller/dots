(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((default-directory (expand-file-name "~"))
	(process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun fg/set-osx-interprogram-functions ()
  "Set interprogram cut/paste functions for local buffers only."
  (unless (and (fboundp 'tramp-tramp-file-p)
               (buffer-file-name)
               (tramp-tramp-file-p (buffer-file-name)))
    ;; Only set interprogram functions if the buffer is not remote.
    (setq-local interprogram-cut-function 'paste-to-osx)
    (setq-local interprogram-paste-function 'copy-from-osx)))

(add-hook 'buffer-list-update-hook #'fg/set-osx-interprogram-functions)
