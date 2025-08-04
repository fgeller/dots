;; -*- lexical-binding: t -*-

(defvar modal-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    map)
  "Map to hold command bindings for `modal-mode'")

(defvar modal-mode-excluded-major-modes '()
  "List of major-modes for which modal-mode should not be activated.")


(defun modal-mode-maybe-activate ()
  (unless (or (minibufferp)
	      (member major-mode modal-mode-excluded-major-modes))
    (modal-mode 1)))

(defun modal-mode-deactivate ()
  (interactive)
  (global-modal-mode -1))

(defun modal-mode-eat-buffer-setup ()
  "Switch eat buffers to appropriate input mode based on modal-mode state."
  (when (eq major-mode 'eat-mode)
    (if modal-mode
        (progn
          (eat-emacs-mode)
          (setq cursor-type 'box))
      (progn
        (eat-semi-char-mode)
        (setq cursor-type nil)))))

(defun modal-mode-activate ()
  (interactive)
  (global-modal-mode 1))

;;;###autoload
(define-minor-mode modal-mode
  "Minor mode that installs a top level command keymap that allows for modal editing.

Available bindings:

\\{modal-mode-map}
"
  :lighter " modal" :keymap modal-mode-map :group 'modal
  (modal-mode-eat-buffer-setup))

;;;###autoload
(define-globalized-minor-mode global-modal-mode
  modal-mode
  modal-mode-maybe-activate
  :group 'modal)

(provide 'modal)

