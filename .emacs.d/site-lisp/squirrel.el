;; -*- lexical-binding: t -*-

;; goals:
;;  - navigation in tree
;;  - navigation to beginning and end of node
;;  - hook to motion command (for marking node)

;; depends on:
;;
;; tree-sitter

(require 'tree-sitter)

(defun squirrel--goto-node (next-node-finder)
  (let* ((current-node (squirrel--ensure-current-node))
		 (next-node (funcall next-node-finder current-node)))
	(when next-node
	  (squirrel--log "motion: current-node=%s " (squirrel--node-to-string current-node))
	  (squirrel--log "motion: next-node=%s " (squirrel--node-to-string next-node))
	  (setq squirrel--current-node next-node)
	  (goto-char (tsc-node-start-position next-node))
	  (run-hooks 'squirrel-motion-hook))))

(defun squirrel-goto-next ()
  (interactive)
  (squirrel--goto-node
   (lambda (c) (tsc-get-next-named-sibling c))))

(defun squirrel-goto-previous ()
  (interactive)
  (squirrel--goto-node
   (lambda (c) (tsc-get-prev-named-sibling c))))

(defun squirrel-goto-parent ()
  (interactive)
  (squirrel--goto-node
   (lambda (c) (tsc-get-parent c))))

(defun squirrel-goto-first-child ()
  (interactive)
  (squirrel--goto-node
   (lambda (c) (tsc-get-nth-named-child c 0))))

;; TODO squirrel-goto-node-start
;; TODO squirrel-goto-node-end

;; TODO remove?
(defun squirrel-mark-current-node ()
  (let* ((c (squirrel--ensure-current-node)))
	(set-mark (tsc-node-end-position c)))) ;; TODO push-mark?

(defun squirrel--point-within-node-p ()
  (let ((p (point)))
	(or (= p (tsc-node-start-position squirrel--current-node))
		(= p (tsc-node-end-position squirrel--current-node)))))

(defun squirrel--ensure-current-node ()
  (when (or (not squirrel--current-node)
			(not (squirrel--point-within-node-p)))
	(setq squirrel--current-node (tree-sitter-node-at-pos :named)))
  (squirrel--log "ensure-current-node: n=[%s]" (squirrel--node-to-string squirrel--current-node))
  squirrel--current-node)

(defvar-local squirrel--current-node nil)

(defconst squirrel-motion-hook nil)

(defconst squirrel--should-log nil)

(defun squirrel--log (msg &rest args)
  (when squirrel--should-log
	(apply 'message (cons (concat ">> squirrel: " msg) args))))

(defun squirrel--node-to-string (n)
  (format "node[type=%s src=%s]"
		  (tsc-node-type n)
		  (squirrel--node-contents n)))

(defun squirrel--node-contents (n)
  (buffer-substring-no-properties (tsc-node-start-position n)
								  (tsc-node-end-position n)))

(defun squirrel--teardown ()
  (setq squirrel--current-node nil))

(defun squirrel--setup ()
  (setq squirrel--current-node nil))

(defconst squirrel-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode squirrel-mode
  "Minor mode that provides navigation and mark functions for tree-sitter supported languages.
"
  :lighter " sq"
  :keymap squirrel-mode-map
  :group 'squirrel
  (tree-sitter--handle-dependent squirrel-mode 'squirrel--setup 'squirrel--teardown))

(provide 'squirrel)
