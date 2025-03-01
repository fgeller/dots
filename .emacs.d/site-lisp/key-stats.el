;;; key-stats.el --- Record statistics of keypresses in a specified mode with simplified JSON storage

;;; Commentary:
;; This package records and aggregates keypresses and associated functions while
;; a specified mode is active and writes the statistics to a simplified JSON file every 5 minutes.
;; Statistics are persisted between sessions by reading existing data from the file.

;;; Code:

(require 'json)

(defconst key-stats-file
  (expand-file-name "key-stats.json" user-emacs-directory)
  "File path for the keypress statistics JSON file.")

(defconst key-stats-interval 300
  "Interval in seconds between writing to the log file.")

(defconst key-stats-target-mode 'modal-mode
  "The mode for which keypress statistics are recorded.")

(defconst key-stats--counts (make-hash-table :test 'equal)
  "Hash table to store keypress counts.  Keys are 'key-command' strings.")

(defconst key-stats--timer nil
  "Timer for periodic logging to file.")

(defconst key-stats--initialized nil
  "Flag indicating whether stats have been loaded from the file.")

(defun key-stats--mode-active-p ()
  "Return non-nil if the target mode is currently active."
  (and (boundp key-stats-target-mode)
       (symbol-value key-stats-target-mode)))

(defun key-stats--load-from-file ()
  "Load existing statistics from the JSON file."
  (when (and (not key-stats--initialized)
             (file-exists-p key-stats-file))
    (condition-case nil
        (let* ((json-array-type 'list)
               (json-key-type 'string)
               (stats (json-read-file key-stats-file)))
          (dolist (entry stats)
            (let ((key (cdr (assoc 'key entry)))
                  (cmd (cdr (assoc 'command entry)))
                  (count (cdr (assoc 'count entry))))
              (puthash (format "%s → %s" key cmd) count key-stats--counts)))
          (message "Loaded existing key statistics from %s" key-stats-file))
      (error (message "Error loading key statistics from %s" key-stats-file))))
  (setq key-stats--initialized t))

(defun key-stats--record-keypress (key)
  "Record KEY and its associated function when pressed in the target mode."
  (when (key-stats--mode-active-p)
    (unless key-stats--initialized (key-stats--load-from-file))
    (let* ((key-description (key-description key))
           (command (key-binding key))
           (command-name (when command (symbol-name command)))
           (key-cmd (format "%s → %s" key-description command-name)))
      (puthash key-cmd
               (1+ (or (gethash key-cmd key-stats--counts) 0))
               key-stats--counts))))

(defun key-stats--write-to-file ()
  "Write aggregated keypress statistics to the JSON file."
  (when (> (hash-table-count key-stats--counts) 0)
    (let ((json-stats '()))
      (maphash (lambda (key-cmd count)
                 ;; Split the key-cmd into separate components
                 (when (string-match "\\(.*\\) → \\(.*\\)" key-cmd)
                   (let ((key (match-string 1 key-cmd))
                         (cmd (match-string 2 key-cmd)))
                     (push `((key . ,key)
                             (command . ,cmd)
                             (count . ,count))
                           json-stats))))
               key-stats--counts)
      (with-temp-file key-stats-file
        (insert (json-encode json-stats))))
    (message "Key statistics logged to %s" key-stats-file)))

(defun key-stats--pre-command-hook ()
  "Trigger key recording when keys are available."
  (when (this-command-keys)
    (key-stats--record-keypress (this-command-keys))))

(defun key-stats-start ()
  "Start recording keypress statistics in the target mode."
  (interactive)
  (key-stats--load-from-file)
  (add-hook 'pre-command-hook 'key-stats--pre-command-hook)

  (setq key-stats--timer
        (run-with-timer key-stats-interval key-stats-interval 'key-stats--write-to-file))
  (message "Key statistics recording started for %s." key-stats-target-mode))

(defun key-stats-stop ()
  "Stop recording keypresses and write final statistics to file."
  (interactive)
  (remove-hook 'pre-command-hook 'key-stats--pre-command-hook)

  (when key-stats--timer
    (cancel-timer key-stats--timer)
    (setq key-stats--timer nil))

  (key-stats--write-to-file)
  (message "Key statistics recording stopped."))

(defun key-stats-view ()
  "View the key statistics in a formatted way."
  (interactive)
  (if (not (file-exists-p key-stats-file))
      (message "No key statistics file found at %s" key-stats-file)
    (let* ((json-array-type 'list)
           (json-key-type 'string)
           (stats (json-read-file key-stats-file))
           (sorted-stats nil))

      ;; Convert to list for sorting
      (dolist (entry stats)
        (let ((key (cdr (assoc "key" entry)))
              (cmd (cdr (assoc "command" entry)))
              (count (cdr (assoc "count" entry))))
		  (message "entry %s key=%s cmd=%s count=%s" entry key cmd count)
          (push (list key cmd count) sorted-stats)))

      ;; Sort by count (descending)
      (setq sorted-stats (sort sorted-stats
                              (lambda (a b) 
                                (> (or (nth 2 a) 0) 
                                   (or (nth 2 b) 0)))))

      ;; Create buffer with formatted view
      (with-current-buffer (get-buffer-create "*Key Statistics*")
        (erase-buffer)
        (insert (format "# Key Statistics for %s\n" key-stats-target-mode))
        (insert (format "# Generated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
        (insert "Key → Function                                   | Count\n")
        (insert "-------------------------------------------------|------\n")

        
        (dolist (stat sorted-stats)
		  (message "stat: %s" stat)
          (let ((key (or (nth 0 stat) "unknown"))
                (cmd (or (nth 1 stat) "unknown"))
                (count (or (nth 2 stat) 0)))
            (insert (format "%-48s | %5d\n" 
                            (format "%s → %s" key cmd) 
                            count))))

        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;;###autoload
(define-minor-mode key-stats-mode
  "Toggle recording of keypress statistics during the target mode."
  :global t
  (if key-stats-mode
      (key-stats-start)
    (key-stats-stop)))

;(key-stats-mode 1)

(provide 'key-stats)
;;; key-stats.el ends here
