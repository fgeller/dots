;;; kubernetes.el --- Emacs porcelain for Kubernetes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1") (dash "2.13.0") (magit-popup "2.8.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'compile)
(require 'dash)
(require 'subr-x)
(require 'magit-popup)

(autoload 'json-pretty-print-buffer "json")
(autoload 'json-read-from-string "json")
(autoload 'org-read-date "org")

(defgroup kubernetes nil
  "Emacs porcelain for Kubernetes."
  :group 'tools
  :prefix "kubernetes-")

(defcustom kubernetes-kubectl-executable "kubectl"
  "The kubectl command used for Kubernetes commands."
  :group 'kubernetes
  :type 'string)

(defcustom kubernetes-display-buffer-select t
  "Whether to select Kubernetes buffers automatically."
  :group 'kubernetes
  :type 'boolean)

(defcustom kubernetes-display-buffer-function #'kubernetes-display-buffer-fullframe
  "The function used display a Kubernetes buffer.

The function must take a single argument, which is the buffer to display."
  :group 'kubernetes
  :type '(radio (function-item kubernetes-display-buffer-fullframe)
                (function-item display-buffer)
                (function :tag "Function")))

(defcustom kubernetes-pod-restart-warning-threshold 5
  "The threshold for pod restarts above which a pod is highlighted."
  :group 'kubernetes
  :type 'number)

(defcustom kubernetes-yaml-indentation-width 2
  "The size of each indentation step in YAML.  Used by the YAML formatter."
  :group 'kubernetes
  :type 'integer)

(defcustom kubernetes-yaml-string-drop-threshold 60
  "The threshold above which a string value will be dropped to the next line."
  :group 'kubernetes
  :type 'integer)

(defcustom kubernetes-refresh-frequency 10
  "The background refresh frequency in seconds."
  :group 'kubernetes
  :type 'integer)

(defcustom kubernetes-json-mode 'javascript-mode
  "The mode to use when rendering pretty-printed JSON."
  :group 'kubernetes
  :type 'function)

(defface kubernetes-context-name
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for context names in report buffers."
  :group 'kubernetes)

(defface kubernetes-section-heading
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :weight bold)
    (((class color) (background  dark)) :foreground "LightGoldenrod2" :weight bold))
  "Face for section headings."
  :group 'kubernetes)

(defface kubernetes-column-heading
  '((((class color) (background light)) :foreground "grey30" :weight bold)
    (((class color) (background  dark)) :foreground "grey80" :weight bold))
  "Face for section headings."
  :group 'kubernetes)

(defface kubernetes-json-key
  '((((class color) (background light)) :foreground "grey30" :weight bold)
    (((class color) (background  dark)) :foreground "grey80" :weight bold))
  "Face for keys in pretty-printed parsed JSON."
  :group 'kubernetes)

(defface kubernetes-progress-indicator
  '((t :inherit shadow))
  "Face for progress indicators."
  :group 'kubernetes)

(defface kubernetes-dimmed
  '((t :inherit shadow))
  "Face for things that shouldn't stand out."
  :group 'kubernetes)

(defface kubernetes-pending-deletion
  '((t :inherit shadow :strike-through t))
  "Face for pods awaiting deletion."
  :group 'kubernetes)

(defface kubernetes-delete-mark
  '((t :inherit error))
  "Face for deletion mark indicators."
  :group 'kubernetes)

(defconst kubernetes-display-pods-buffer-name "*kubernetes pods*")

(defconst kubernetes-display-config-buffer-name "*kubernetes config*")

(defconst kubernetes-log-line-buffer-name "*log line*")

(defconst kubernetes-logs-buffer-name "*kubernetes logs*")

(defconst kubernetes-pod-buffer-name "*kubernetes pod*")


;; Main Kubernetes query routines

(defun kubernetes--kubectl-default-error-handler (buf)
  (with-current-buffer buf
    (error "Kubectl failed.  Reason: %s" (buffer-string))))

(defun kubernetes--kubectl (args on-success &optional on-error)
  "Run kubectl with ARGS.

ON-SUCCESS is a function of one argument, called with the process' buffer.

ON-ERROR is a function of one argument, called with the process'
buffer.  If omitted, it defaults to
`kubernetes--kubectl-default-error-handler', which raises an
error.

Returns the process object for this execution of kubectl."
  (let* ((buf (generate-new-buffer " kubectl"))
         (process (apply #'start-process "kubectl" buf kubernetes-kubectl-executable args))
         (sentinel
          (lambda (proc _status)
            (cond
             ((zerop (process-exit-status proc))
              (funcall on-success buf))
             (t
              (cond (on-error
                     (message "Kubectl failed.  Reason: %s"
                              (with-current-buffer buf
                                (buffer-string)))
                     (funcall on-error buf))

                    (t
                     (kubernetes--kubectl-default-error-handler (process-buffer proc)))))))))
    (set-process-sentinel process sentinel)
    process))

;;;###autoload
(defun kubernetes-get-pods (cb &optional cleanup-cb)
  "Get all pods and execute callback CB with the parsed JSON.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (kubernetes--kubectl '("get" "pods" "-o" "json")
             (lambda (buf)
               (unwind-protect
                   (let ((json (with-current-buffer buf
                                 (json-read-from-string (buffer-string)))))
                     (funcall cb json))
                 (funcall (or cleanup-cb #'ignore))))))

;;;###autoload
(defun kubernetes-config-view (cb &optional cleanup-cb)
  "Get the current configuration and pass it to CB.

CLEANUP-CB is a function taking no arguments used to release any resources."
  (kubernetes--kubectl '("config" "view" "-o" "json")
             (lambda (buf)
               (unwind-protect
                   (let ((json (with-current-buffer buf
                                 (json-read-from-string (buffer-string)))))
                     (funcall cb json))
                 (funcall (or cleanup-cb #'ignore))))))

;;;###autoload
(defun kubernetes-delete-pod (pod-name cb &optional error-cb)
  "Delete pod with POD-NAME, then execute CB with the response buffer.

ERROR-CB is called if an error occurred."
  (kubernetes--kubectl (list "delete" "pod" pod-name "-o" "name")
             (lambda (buf)
               (with-current-buffer buf
                 (string-match (rx bol "pod/" (group (+ nonl))) (buffer-string))
                 (funcall cb (match-string 1 (buffer-string)))))
             error-cb))

;;;###autoload
(defun kubernetes-kubectl-describe-pod (pod-name cb)
  "Describe pod with POD-NAME, then execute CB with the string response."
  (kubernetes--kubectl (list "describe" "pod" pod-name)
             (lambda (buf)
               (let ((s (with-current-buffer buf (buffer-string))))
                 (funcall cb s)))))


(defun kubernetes--await-on-async (fn)
  "Turn an async function requiring a callback into a synchronous one.

Transforms a function of type:

  FN : (a -> b) -> process

to a function of the type:

  FN' : () -> a"
  (let* (complete result)
    (funcall fn (lambda (response)
                  (setq complete t)
                  (setq result response)))

    (while (not complete)
      (sleep-for 0.001))

    result))


;; View management

(defun kubernetes-display-buffer-fullframe (buffer)
  (let ((display-fn
         (lambda (buffer alist)
           (when-let (window (or (display-buffer-reuse-window buffer alist)
                                 (display-buffer-same-window buffer alist)
                                 (display-buffer-pop-up-window buffer alist)
                                 (display-buffer-use-some-window buffer alist)))
             (delete-other-windows window)
             window))))
    (display-buffer buffer (list display-fn))))

(defun kubernetes-display-buffer (buffer)
  (let ((window (funcall kubernetes-display-buffer-function buffer)))
    (when kubernetes-display-buffer-select
      (select-frame-set-input-focus
       (window-frame (select-window window))))))

(defun kubernetes-navigate (point)
  "Perform a context-sensitive navigation action.

Inspecs the `kubernetes-nav' text property at POINT to determine
how to navigate.  If that property is not found, no action is
taken."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (`(:config ,config)
     (kubernetes-display-config config))
    (`(:pod ,pod)
     (kubernetes-display-pod pod))))

(defun kubernetes-copy-thing-at-point (point)
  "Perform a context-sensitive copy action.

Inspecs the `kubernetes-copy' text property at POINT to determine
what to copy."
  (interactive "d")
  (when-let (s (get-text-property point 'kubernetes-copy))
    (kill-new s)
    (message "Copied: %s" s)))

(defun kubernetes--json-to-yaml (json &optional level)
  (let* ((level (or level 0))
         (space (string-to-char " "))
         (indentation (make-string (* level kubernetes-yaml-indentation-width) space))
         (body
          (cond
           ((vectorp json)
            (let* ((list-items (--map (string-trim-left (kubernetes--json-to-yaml it (1+ level)))
                                      (append json nil)))
                   (separator (concat "\n"
                                      indentation "-" "\n"
                                      indentation "  "))
                   (joined (string-join list-items separator)))
              ;; If this is an empty or singleton list, do not drop.
              (if (<= (length list-items) 1)
                  (concat indentation "- " (string-trim-right joined))
                (concat indentation "- \n"
                        indentation "  " (string-trim-right joined)))))
           ((listp json)
            (let ((entries (--map
                            (-let [(k . v) it]
                              (concat indentation
                                      (propertize (format "%s: " (symbol-name k)) 'face 'kubernetes-json-key)
                                      (cond
                                       ((equal t v) "true")
                                       ((equal nil v) "false")
                                       ((numberp v) (number-to-string v))
                                       ((and (stringp v) (< (length v) kubernetes-yaml-string-drop-threshold)) v)
                                       (t
                                        (concat "\n" (kubernetes--json-to-yaml v (1+ level)))))))
                            json)))
              (string-join entries "\n")))
           (t
            (format "%s%s" indentation json)))))
    (if (= 0 level)
        (concat (propertize "---\n" 'face 'kubernetes-dimmed) body)
      body)))


;;; Displaying config

(defun kubernetes-display-config-refresh (config)
  (let ((buf (get-buffer-create kubernetes-display-config-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-config-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (kubernetes--json-to-yaml config))))
    buf))

;;;###autoload
(defun kubernetes-display-config (config)
  "Display information for CONFIG in a new window."
  (interactive (list (kubernetes--await-on-async #'kubernetes-config-view)))
  (with-current-buffer (kubernetes-display-config-refresh config)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))

(defvar kubernetes-display-config-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") #'quit-window)
    (define-key keymap (kbd "RET") #'kubernetes-navigate)
    (define-key keymap (kbd "M-w") #'kubernetes-copy-thing-at-point)
    keymap)
  "Keymap for `kubernetes-display-config-mode'.")

(define-derived-mode kubernetes-display-config-mode special-mode "Kubernetes Config"
  "Mode for inspecting a Kubernetes config.

\\{kubernetes-display-config-mode-map}"
  :group 'kubernetes
  (read-only-mode +1))


;;; Displaying a specific pod

(defvar kubernetes--pods-response nil
  "Cache of last pods response received over the API.")

(defun kubernetes--read-pod ()

  (-let* (((&alist 'items pods)
           (or kubernetes--pods-response
               (progn
                 (message "Getting pods...")
                 (kubernetes--await-on-async #'kubernetes-get-pods))))
          (pods (append pods nil))
          (podname (-lambda ((&alist 'metadata (&alist 'name name)))
                     name))
          (names (-map podname pods))
          (choice (completing-read "Pod: " names nil t)))
    (--find (equal choice (funcall podname it)) pods)))

(defun kubernetes-display-pod-refresh (pod)
  (let ((buf (get-buffer-create kubernetes-pod-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-pod-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (kubernetes--json-to-yaml pod))))
    buf))

(defvar kubernetes-display-pod-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") #'quit-window)
    (define-key keymap (kbd "RET") #'kubernetes-navigate)
    (define-key keymap (kbd "M-w") #'kubernetes-copy-thing-at-point)
    keymap)
  "Keymap for `kubernetes-display-pod-mode'.")

(define-derived-mode kubernetes-display-pod-mode special-mode "Kubernetes Pod"
  "Mode for inspecting a Kubernetes pod.

\\{kubernetes-display-pod-mode-map}"
  :group 'kubernetes
  (read-only-mode +1))


;;;###autoload
(defun kubernetes-display-pod (pod)
  "Display information for POD in a new window."
  (interactive (list (kubernetes--read-pod)))
  (with-current-buffer (kubernetes-display-pod-refresh pod)
    (goto-char (point-min))
    (select-window (display-buffer (current-buffer)))))


;;; Displaying all pods

;; Marker variables used track buffer locations to update.
(defvar kubernetes--config-start-marker nil)
(defvar kubernetes--config-end-marker nil)
(defvar kubernetes--pod-count-marker nil)
(defvar kubernetes--pods-start-marker nil)
(defvar kubernetes--pods-end-marker nil)

;; Context section rendering.

(defvar kubernetes--view-config-process nil
  "Single process used to prevent concurrent config refreshes.")

(defun kubernetes--set-view-config-process (proc)
  (kubernetes--release-view-config-process)
  (setq kubernetes--view-config-process proc))

(defun kubernetes--release-view-config-process ()
  (when-let (proc kubernetes--view-config-process)
    (when (process-live-p proc)
      (kill-process proc)))
  (setq kubernetes--view-config-process nil))


(defun kubernetes--format-context-section (config)
  (with-temp-buffer
    (-let [(&alist 'current-context current 'contexts contexts) config]
      (insert (propertize (concat
                           (format "%-12s" "Context: ")
                           (propertize (or current "<none>") 'face 'kubernetes-context-name))
                          'kubernetes-copy current))
      (newline)

      (-when-let* ((ctx (--find (equal current (alist-get 'name it)) (append contexts nil)))
                   ((&alist 'name n 'context (&alist 'cluster c 'namespace ns)) ctx))
        (unless (string-empty-p c)
          (insert (propertize (format "%-12s%s" "Cluster: " c)
                              'kubernetes-copy c))
          (newline))

        (unless (string-empty-p ns)
          (insert (propertize (format "%-12s%s" "Namespace: " ns)
                              'kubernetes-copy ns))
          (newline))))

    (propertize (buffer-string) 'kubernetes-nav (list :config config))))

(defun kubernetes--redraw-context-section (start-marker end-marker config)
  (when (and (buffer-live-p (marker-buffer start-marker))
             (buffer-live-p (marker-buffer end-marker)))
    (with-current-buffer (marker-buffer start-marker)
      (save-excursion
        (goto-char (marker-position start-marker))
        (let ((inhibit-read-only t))
          (delete-region (point) (1- (marker-position end-marker)))
          (insert (kubernetes--format-context-section config)))))))

(defun kubernetes--initialize-context-section ()
  (set-marker kubernetes--config-start-marker (point))

  (kubernetes--set-view-config-process
   (kubernetes-config-view (lambda (config)
                   (kubernetes--redraw-context-section kubernetes--config-start-marker kubernetes--config-end-marker config))
                 (lambda ()
                   (kubernetes--release-view-config-process))))
  (insert " ")
  (set-marker kubernetes--config-end-marker (point)))

(defun kubernetes--refresh-context-section ()
  (unless kubernetes--view-config-process
    (kubernetes--set-view-config-process
     (kubernetes-config-view (lambda (config)
                     (kubernetes--redraw-context-section kubernetes--config-start-marker kubernetes--config-end-marker config))
                   (lambda ()
                     (kubernetes--release-view-config-process))))))


;; Pod section rendering.

(defvar kubernetes--get-pods-process nil
  "Single process used to prevent concurrent get pods requests.")

(defun kubernetes--set-get-pods-process (proc)
  (kubernetes--release-get-pods-process)
  (setq kubernetes--get-pods-process proc))

(defun kubernetes--release-get-pods-process ()
  (when-let (proc kubernetes--get-pods-process)
    (when (process-live-p proc)
      (kill-process proc)))
  (setq kubernetes--get-pods-process nil))


(defvar kubernetes--refresh-timer nil
  "Background timer used to poll for updates.")

(defvar-local kubernetes--marked-pod-names nil)
(defvar-local kubernetes--pods-pending-deletion nil)

(defun kubernetes--ellipsize (s threshold)
  (if (> (length s) threshold)
      (concat (substring s 0 (1- threshold)) "…")
    s))

(defun kubernetes--parse-utc-timestamp (s)
  (let ((parsed (parse-time-string (replace-regexp-in-string "Z" "" (replace-regexp-in-string "T" " " s)))))
    (setf (nth 8 parsed) 0)
    parsed))

(defun kubernetes--time-diff-string (start now)
  (let ((diff (time-to-seconds (time-subtract now start))))
    (car (split-string (format-seconds "%yy,%dd,%hh,%mm,%ss%z" diff) ","))))

(defun kubernetes--format-pod-line (pod)
  (-let* (((&alist 'metadata (&alist 'name name)
                   'status (&alist 'containerStatuses [(&alist 'restartCount restarts
                                                               'state state)]
                                   'startTime start-time
                                   'phase phase))
           pod)
          (state (or (alist-get 'reason (alist-get 'waiting state))
                     phase))
          (str
           (concat (format "%-45s " (kubernetes--ellipsize name 45))
                   (let ((s (format "%-10s " (kubernetes--ellipsize state 10))))
                     (if (equal state "Running") (propertize s 'face 'kubernetes-dimmed) s))
                   (let ((s (format "%8s " restarts)))
                     (cond
                      ((equal 0 restarts)
                       (propertize s 'face 'kubernetes-dimmed))
                      ((<= kubernetes-pod-restart-warning-threshold restarts)
                       (propertize s 'face 'warning))
                      (t
                       s)))
                   (let* ((start (apply #'encode-time (kubernetes--parse-utc-timestamp start-time)))
                          (now (current-time)))
                     (propertize (format "%8s" (kubernetes--time-diff-string start now))
                                 'face 'kubernetes-dimmed))))
          (str (cond
                ((member (downcase state) '("running" "containercreating" "terminated"))
                 str)
                ((member (downcase state) '("runcontainererror" "crashloopbackoff"))
                 (propertize str 'face 'error))
                (t
                 (propertize str 'face 'warning))))
          (str
           (if (member name kubernetes--pods-pending-deletion)
               (concat (propertize str 'face 'kubernetes-pending-deletion))
             str))
          (str
           (if (member name kubernetes--marked-pod-names)
               (concat (propertize "D" 'face 'kubernetes-delete-mark) " " str)
             (concat "  " str))))
    (propertize str
                'kubernetes-nav (list :pod pod)
                'kubernetes-copy name)))

(defun kubernetes--clean-pod-state-vars (pods)
  (let ((pod-names
         (-map (-lambda ((&alist 'metadata (&alist 'name name)))
                 name)
               pods)))
    (setq kubernetes--pods-pending-deletion
          (-intersection kubernetes--pods-pending-deletion pod-names))
    (setq kubernetes--marked-pod-names
          (-intersection kubernetes--marked-pod-names pod-names))))


(defun kubernetes--redraw-pods-section (count-marker pods-start-marker pods-end-marker pods)
  (when (and (buffer-live-p (marker-buffer count-marker))
             (buffer-live-p (marker-buffer pods-start-marker))
             (buffer-live-p (marker-buffer pods-end-marker)))
    (-let [(&alist 'items pods) pods]
      (with-current-buffer (marker-buffer count-marker)
        (let ((pos (point)))
          (save-excursion
            (goto-char (marker-position count-marker))
            (let ((inhibit-read-only t))
              (delete-region (point) (line-end-position))
              (insert (format "(%s)" (length pods)))))
          (save-excursion
            (goto-char (marker-position pods-start-marker))
            (let ((inhibit-read-only t))
              (delete-region (point) (1- (marker-position pods-end-marker)))
              (--each (append pods nil)
                (insert (kubernetes--format-pod-line it))
                (newline))))
          (kubernetes--clean-pod-state-vars pods)
          (goto-char pos))))))

(defun kubernetes--initialize-pods-section ()
  (insert (propertize "Pods " 'face 'kubernetes-section-heading))
  (set-marker kubernetes--pod-count-marker (point))
  (newline)
  (insert (propertize (format "  %-45s %-10s %8s %8s\n" "Name" "Status" "Restarts" "Age")
                      'face 'kubernetes-column-heading))
  (set-marker kubernetes--pods-start-marker (point))
  (insert (propertize "  Fetching... " 'face 'kubernetes-progress-indicator))
  (set-marker kubernetes--pods-end-marker (point))

  (kubernetes--set-get-pods-process
   (kubernetes-get-pods
    (lambda (response)
      (setq kubernetes--pods-response response)
      (kubernetes--redraw-pods-section kubernetes--pod-count-marker kubernetes--pods-start-marker kubernetes--pods-end-marker response))
    (lambda ()
      (kubernetes--release-get-pods-process))))

  (newline))

(defun kubernetes--refresh-pods-section ()
  (unless kubernetes--get-pods-process
    (kubernetes--set-get-pods-process
     (kubernetes-get-pods
      (lambda (response)
        (setq kubernetes--pods-response response)
        (kubernetes--redraw-pods-section kubernetes--pod-count-marker kubernetes--pods-start-marker kubernetes--pods-end-marker response))
      (lambda ()
        (kubernetes--release-get-pods-process))))))


;; Root rendering routines.

(defun kubernetes-display-pods-initialize-buffer ()
  (let ((buf (get-buffer-create kubernetes-display-pods-buffer-name)))
    (with-current-buffer buf
      (kubernetes-display-pods-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (kubernetes--initialize-context-section)
        (newline)
        (kubernetes--initialize-pods-section))
      (goto-char (point-min))

      ;; Initialize refresh timer.
      (setq kubernetes--refresh-timer (run-with-timer kubernetes-refresh-frequency kubernetes-refresh-frequency #'kubernetes-display-pods-refresh))
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when-let (timer kubernetes--refresh-timer)
                    (setq kubernetes--refresh-timer nil)
                    (cancel-timer timer)))
                nil t))
    buf))

;;;###autoload
(defun kubernetes-display-pods-refresh ()
  "Refresh the Kubernetes pods buffer."
  (interactive)
  (if-let (buf (get-buffer kubernetes-display-pods-buffer-name))
      (progn
        (kubernetes--refresh-context-section)
        (kubernetes--refresh-pods-section)
        buf)
    (error "Attempted to refresh kubernetes pods buffer, but it does not exist")))

(defvar kubernetes-display-pods-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "g") #'kubernetes-display-pods-refresh)
    (define-key keymap (kbd "q") #'quit-window)
    (define-key keymap (kbd "RET") #'kubernetes-navigate)
    (define-key keymap (kbd "d") #'kubernetes-describe-pod)
    (define-key keymap (kbd "M-w") #'kubernetes-copy-thing-at-point)
    (define-key keymap (kbd "D") #'kubernetes-mark-for-delete)
    (define-key keymap (kbd "u") #'kubernetes-unmark)
    (define-key keymap (kbd "U") #'kubernetes-unmark-all)
    (define-key keymap (kbd "x") #'kubernetes-execute-marks)
    (define-key keymap (kbd "l") #'kubernetes-logs)
    (define-key keymap (kbd "h") #'describe-mode)
    keymap)
  "Keymap for `kubernetes-display-pods-mode'.")

(define-derived-mode kubernetes-display-pods-mode special-mode "Kubernetes Pods"
  "Mode for working with Kubernetes pods.

\\<kubernetes-display-pods-mode-map>\
Type \\[kubernetes-mark-for-delete] to mark a pod for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the pod at point, or \\[kubernetes-unmark-all] to unmark all pods.

Type \\[kubernetes-navigate] to inspect the object on the current line, and \\[kubernetes-describe-pod] to
specifically describe a pod.

Type \\[kubernetes-logs] when point is on a pod to view its logs.

Type \\[kubernetes-copy-thing-at-point] to copy the pod name at point.

Type \\[kubernetes-display-pods-refresh] to refresh the buffer.

\\{kubernetes-display-pods-mode-map}"
  :group 'kubernetes
  (read-only-mode +1)
  (setq kubernetes--config-start-marker (make-marker))
  (setq kubernetes--config-end-marker (make-marker))
  (setq kubernetes--pod-count-marker (make-marker))
  (setq kubernetes--pods-start-marker (make-marker))
  (setq kubernetes--pods-end-marker (make-marker)))

;;;###autoload
(defun kubernetes-display-pods ()
  "Display a list of pods in the current Kubernetes context."
  (interactive)
  (let ((buf (if (get-buffer kubernetes-display-pods-buffer-name)
                 (kubernetes-display-pods-refresh)
               (kubernetes-display-pods-initialize-buffer))))
    (kubernetes-display-buffer buf)
    (message (substitute-command-keys
              "\\<kubernetes-display-pods-mode-map>Type \\[describe-mode] for usage."))))

;; Marked pod state management.

(defun kubernetes-mark-for-delete (point)
  "Mark the thing at POINT for deletion, then advance to the next line."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (`(:pod ,pod)
     (-let [(&alist 'metadata (&alist 'name name)) pod]
       (unless (member name kubernetes--pods-pending-deletion)
         (add-to-list 'kubernetes--marked-pod-names name)
         (kubernetes--redraw-pods-section kubernetes--pod-count-marker
                                kubernetes--pods-start-marker
                                kubernetes--pods-end-marker
                                kubernetes--pods-response))))
    (_
     (user-error "Nothing here can be marked")))
  (goto-char point)
  (forward-line 1))

(defun kubernetes-unmark (point)
  "Unmark the thing at POINT, then advance to the next line."
  (interactive "d")
  (pcase (get-text-property point 'kubernetes-nav)
    (`(:pod ,pod)
     (-let [(&alist 'metadata (&alist 'name name)) pod]
       (setq kubernetes--marked-pod-names (delete name kubernetes--marked-pod-names))
       (kubernetes--redraw-pods-section kubernetes--pod-count-marker
                              kubernetes--pods-start-marker
                              kubernetes--pods-end-marker
                              kubernetes--pods-response))))
  (goto-char point)
  (forward-line 1))

(defun kubernetes-unmark-all ()
  "Unmark everything in the buffer."
  (interactive)
  (setq kubernetes--marked-pod-names nil)
  (let ((pt (point)))
    (kubernetes--redraw-pods-section kubernetes--pod-count-marker
                           kubernetes--pods-start-marker
                           kubernetes--pods-end-marker
                           kubernetes--pods-response)
    (goto-char pt)))

(defun kubernetes-execute-marks ()
  "Action all marked items in the buffer."
  (interactive)
  (unless kubernetes--marked-pod-names
    (user-error "Nothing is marked"))

  (let ((n (length kubernetes--marked-pod-names)))
    (if (y-or-n-p (format "Execute %s mark%s? " n (if (equal 1 n) "" "s")))
        (progn
          (message "Deleting %s pod%s..." n (if (equal 1 n) "" "s"))
          (dolist (pod kubernetes--marked-pod-names)
            (add-to-list 'kubernetes--pods-pending-deletion pod)

            (kubernetes-delete-pod pod
                         (lambda (_)
                           (message "Deleting pod %s succeeded." pod)
                           (kubernetes-display-pods-refresh))
                         (lambda (_)
                           (message "Deleting pod %s failed" pod)
                           (setq kubernetes--pods-pending-deletion (delete pod kubernetes--pods-pending-deletion)))))

          (kubernetes-unmark-all))
      (message "Cancelled."))))


;; Logs

(defvar kubernetes-logs-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "n") #'kubernetes-logs-forward-line)
    (define-key keymap (kbd "p") #'kubernetes-logs-previous-line)
    (define-key keymap (kbd "RET") #'kubernetes-logs-inspect-line)
    keymap)
  "Keymap for `kubernetes-logs-mode'.")

(define-compilation-mode kubernetes-logs-mode "Kubernetes Logs"
  "Mode for displaying and inspecting Kubernetes logs.

\\<kubernetes-logs-mode-map>\
Type \\[kubernetes-logs-inspect-line] to open the line at point in a new buffer.

\\{kubernetes-logs-mode-map}")

(defvar kubernetes-log-line-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "n") #'kubernetes-logs-forward-line)
    (define-key keymap (kbd "p") #'kubernetes-logs-previous-line)
    (define-key keymap (kbd "q") #'quit-window)
    keymap)
  "Keymap for `kubernetes-log-line-mode'.")

(define-compilation-mode kubernetes-log-line-mode "Log Line"
  "Mode for inspecting Kubernetes log lines.

\\{kubernetes-log-line-mode-map}"
  (read-only-mode))

(defun kubernetes--log-line-buffer-for-string (s)
  (let ((propertized (with-temp-buffer
                       (insert s)
                       (goto-char (point-min))
                       (when (equal (char-after) ?\{)
                         (json-pretty-print-buffer)
                         (funcall kubernetes-json-mode)
                         (font-lock-ensure))
                       (buffer-string))))

    (with-current-buffer (get-buffer-create kubernetes-log-line-buffer-name)
      (kubernetes-log-line-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert propertized)
        (goto-char (point-min)))
      (current-buffer))))

(defun kubernetes-logs-inspect-line (pos)
  "Show detail for the log line at POS."
  (interactive "d")
  (display-buffer (kubernetes--log-line-buffer-for-string
                   (save-excursion
                     (goto-char pos)
                     (buffer-substring (line-beginning-position) (line-end-position))))))

(defun kubernetes-logs-previous-line ()
  "Move backward and inspect the line at point."
  (interactive)
  (with-current-buffer kubernetes-logs-buffer-name
    (forward-line -1)
    (when (get-buffer kubernetes-log-line-buffer-name)
      (kubernetes-logs-inspect-line (point)))))

(defun kubernetes-logs-forward-line ()
  "Move forward and inspect the line at point."
  (interactive)
  (with-current-buffer kubernetes-logs-buffer-name
    (forward-line 1)
    (when (get-buffer kubernetes-log-line-buffer-name)
      (kubernetes-logs-inspect-line (point)))))


;; Logs popup

(defvar kubernetes--pod-to-log nil
  "Identifies the pod to log after querying the user for flags.

Assigned before opening the logging popup, when the target pod is
likely to be at point.  After choosing flags, this is the pod that
will be logged.

This variable is reset after use by the logging functions.")

;;;###autoload
(defun kubernetes-logs (pod)
  "Popup console for logging commands for POD."
  (interactive (list (or (kubernetes--maybe-pod-at-point) (kubernetes--read-pod))))
  (setq kubernetes--pod-to-log pod)
  (call-interactively #'kubernetes-logs-popup))

(magit-define-popup kubernetes-logs-popup
  "Popup console for logging commands for POD."
  :variable 'kubernetes-logs-arguments
  :group 'kubernetes

  :options
  '("Options for customizing logging behaviour"
    (?t "Number of lines to display" "--tail=" read-number "-1")
    (?s "Since relative time" "--since=" kubernetes--read-time-value)
    (?d "Since absolute datetime" "--since-time=" kubernetes--read-iso-datetime))

  :actions
  '((?l "Logs" kubernetes-logs-fetch-all)
    (?f "Logs (stream and follow)" kubernetes-logs-follow))

  :default-action 'kubernetes-logs)

(defun kubernetes--read-iso-datetime (&rest _)
  (let* ((date (org-read-date nil t))
         (tz (format-time-string "%z" date)))
    (concat
     (format-time-string "%Y-%m-%dT%H:%M:%S" date)
     (replace-regexp-in-string (rx (group (? (any "+-")) digit digit)
                                   (group digit digit))
                               "\\1:\\2"
                               tz))))

(defun kubernetes--read-time-value (&rest _)
  (let (result)
    (while (null result)
      (let ((input (read-string "Time value (e.g. 20s): ")))
        (if (string-match-p (rx bol (* space) (+ digit) (* space) (any "smh") (* space) eol)
                            input)
            (setq result input)
          (message "Invalid time value")
          (sit-for 1))))
    result))

(defun kubernetes--maybe-pod-at-point ()
  (pcase (get-text-property (point) 'kubernetes-nav)
    (`(:pod ,pod)
     pod)))

;;;###autoload
(defun kubernetes-logs-follow ()
  "Open a streaming logs buffer for a pod.

Should be invoked via `kubernetes-logs-popup'."
  (interactive)
  (kubernetes-logs-fetch-all (cons "-f" (kubernetes-logs-arguments))))

;;;###autoload
(defun kubernetes-logs-fetch-all (args)
  "Open a streaming logs buffer for a pod.

ARGS are additional args to pass to kubectl.

Should be invoked via `kubernetes-logs-popup'."
  (interactive (list (kubernetes-logs-arguments)))
  (-let* (((&alist 'metadata (&alist 'name pod-name)) kubernetes--pod-to-log)
          (compilation-buffer-name-function (lambda (_) kubernetes-logs-buffer-name))
          (command (-flatten (list kubernetes-kubectl-executable "logs" args pod-name))))
    (setq kubernetes--pod-to-log nil)
    (with-current-buffer (compilation-start (string-join command " ") 'kubernetes-logs-mode)
      (setq-local compilation-error-regexp-alist nil)
      (setq-local compilation-error-regexp-alist-alist nil)
      (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
      (display-buffer (current-buffer)))))


;; Describing pods

;;;###autoload
(defun kubernetes-describe-pod (pod)
  "Popup buffer for describing POD."
  (interactive (list (or (kubernetes--maybe-pod-at-point) (kubernetes--read-pod))))
  (kubernetes-display-buffer (kubernetes--initialize-describe-pod-buffer pod)))

(defun kubernetes--initialize-describe-pod-buffer (pod)
  (-let ((buf (get-buffer-create kubernetes-pod-buffer-name))
         ((&alist 'metadata (&alist 'name pod-name)) pod)
         (marker (make-marker)))
    (with-current-buffer buf
      (kubernetes-display-pod-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (set-marker marker (point))
        (insert (propertize "Loading..." 'face 'kubernetes-dimmed))))
    (kubernetes-kubectl-describe-pod pod-name
                           (lambda (s)
                             (with-current-buffer (marker-buffer marker)
                               (setq-local tab-width 8)
                               (let ((inhibit-read-only t)
                                     (inhibit-redisplay t))
                                 (erase-buffer)
                                 (insert "---\n")
                                 (insert s)
                                 (untabify (point-min) (point-max))
                                 (goto-char (point-min))))))
    buf))


(provide 'kubernetes)

;;; kubernetes.el ends here
