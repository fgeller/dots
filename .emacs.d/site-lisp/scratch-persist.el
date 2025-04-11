;;; scratch-persist.el --- Automatically persist the *scratch* buffer  -*- lexical-binding: t; -*-

;; Keywords: convenience, scratch

;;; Commentary:

;; This package automatically persists the *scratch* buffer to a file
;; and restores it on Emacs startup.
;;
;; Overview:
;; - Set a custom location for persisted files
;; - Automatically load the last persisted scratch buffer on startup
;; - Persist the buffer every minute with a single timer
;; - Customize the filename used for persistence
;; - Use a default filename function including the current date
;; - Skip persisting empty scratch buffers

;;; Code:

(require 'cl-lib)

(defgroup scratch-persist nil
  "Automatically persist the *scratch* buffer."
  :group 'convenience
  :prefix "scratch-persist-")

(defcustom scratch-persist-directory (expand-file-name "scratch-persist" user-emacs-directory)
  "Directory where scratch buffers are persisted."
  :type 'directory
  :group 'scratch-persist)

(defcustom scratch-persist-filename-function #'scratch-persist-default-filename
  "Function that returns the filename for the persisted scratch buffer.
The function should take no arguments and return a string."
  :type 'function
  :group 'scratch-persist)

(defcustom scratch-persist-interval 60
  "Interval in seconds to persist the scratch buffer."
  :type 'integer
  :group 'scratch-persist)

(defcustom scratch-persist-load-on-startup t
  "Whether to load the persisted scratch buffer on startup."
  :type 'boolean
  :group 'scratch-persist)

(defvar scratch-persist--timer nil
  "Timer for persisting the scratch buffer.")

(defun scratch-persist-default-filename ()
  "Generate a default filename for the persisted scratch buffer.
The filename includes the current date in the format 'scratch-YYYY-MM-DD.el'."
  (format "scratch-%s.el" (format-time-string "%Y-%m-%d")))

(defun scratch-persist--ensure-directory ()
  "Ensure the persistence directory exists."
  (unless (file-exists-p scratch-persist-directory)
    (make-directory scratch-persist-directory t)))

(defun scratch-persist--make-filepath ()
  "Make the full path for the persisted scratch buffer."
  (expand-file-name (funcall scratch-persist-filename-function) scratch-persist-directory))

(defun scratch-persist-save ()
  "Save the current scratch buffer to a file if it's not empty."
  (interactive)
  (when (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (let ((content (buffer-string)))
        (when (and content (not (string-empty-p (string-trim content))))
          (scratch-persist--ensure-directory)
          (let ((filepath (scratch-persist--make-filepath)))
            (write-region content nil filepath)
            (when (called-interactively-p 'any)
              (set-buffer-modified-p nil))
            (message "Scratch buffer persisted to %s" filepath)))))))

(defun scratch-persist-load ()
  "Load the persisted scratch buffer."
  (interactive)
  (let ((filepath (scratch-persist--make-filepath)))
    (if (file-exists-p filepath)
        (with-current-buffer "*scratch*"
          (erase-buffer)
          (insert-file-contents filepath)
          (message "Loaded persisted scratch buffer from %s" filepath))
      (message "No persisted scratch buffer found at %s" filepath))))

(defun scratch-persist-find-latest ()
  "Find the latest persisted scratch file."
  (scratch-persist--ensure-directory)
  (let ((files (directory-files scratch-persist-directory t "scratch-.*\\.el$")))
    (car (sort files (lambda (a b)
                       (time-less-p (nth 5 (file-attributes b))
                                    (nth 5 (file-attributes a))))))))

(defun scratch-persist-load-latest ()
  "Load the latest persisted scratch buffer."
  (interactive)
  (let ((latest-file (scratch-persist-find-latest)))
	(message "found latest file %s" latest-file)
    (if latest-file
        (with-current-buffer "*scratch*"
          (erase-buffer)
          (insert-file-contents latest-file)
          (message "Loaded latest persisted scratch buffer from %s" latest-file))
      (message "No persisted scratch buffer found"))))

(defun scratch-persist-start-timer ()
  "Start or restart the timer for persisting the scratch buffer."
  (scratch-persist-stop-timer)
  (setq scratch-persist--timer
        (run-with-timer scratch-persist-interval scratch-persist-interval
                        #'scratch-persist-save)))

(defun scratch-persist-stop-timer ()
  "Stop the timer for persisting the scratch buffer."
  (when scratch-persist--timer
    (cancel-timer scratch-persist--timer)
    (setq scratch-persist--timer nil)))

(defun scratch-persist-advice-save-buffer (&rest _args)
  "Advice function to make save-buffer save to the persisted file in *scratch* buffer."
  (when (string= (buffer-name) "*scratch*")
    (scratch-persist-save)
    t))

(defun scratch-persist-before-exit ()
  "Save scratch buffer before Emacs exits."
  (when scratch-persist-mode
    (scratch-persist-save)))

;;;###autoload
(define-minor-mode scratch-persist-mode
  "Toggle automatic persistence of the *scratch* buffer."
  :global t
  :group 'scratch-persist
  (if scratch-persist-mode
      (progn
        (scratch-persist-start-timer)
        (add-hook 'kill-emacs-hook #'scratch-persist-before-exit)
        (advice-add 'save-buffer :before-until #'scratch-persist-advice-save-buffer)
        (when scratch-persist-load-on-startup
          (scratch-persist-load-latest)))
    (progn
      (scratch-persist-stop-timer)
      (remove-hook 'kill-emacs-hook #'scratch-persist-before-exit)
      (advice-remove 'save-buffer #'scratch-persist-advice-save-buffer))))

(provide 'scratch-persist)
;;; scratch-persist.el ends here
