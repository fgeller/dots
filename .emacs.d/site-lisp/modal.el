;; -*- lexical-binding: t -*-

(defvar modal-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    map)
  "Map to hold command bindings for `modal-mode'")

(defvar modal-mode-excluded-major-modes '()
  "List of major-modes for which modal-mode should not be activated.")

(defvar modal-mode-emulation-alist nil
  "Keymap alist for modal-mode with emulation-mode precedence.")

(defvar modal-mode-high-precedence-modes '(eat-mode)
  "Major modes that require modal-mode keymap to have higher precedence.")

(defun modal-mode-maybe-activate ()
  (unless (or (minibufferp)
	      (member major-mode modal-mode-excluded-major-modes))
    (modal-mode 1)))

(defun modal-mode-deactivate ()
  (interactive)
  (global-modal-mode -1))

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
  (if modal-mode
      (when (member major-mode modal-mode-high-precedence-modes)
        ;; Use emulation-mode-map-alists for higher precedence
        (setq modal-mode-emulation-alist `((modal-mode . ,modal-mode-map)))
        (add-to-list 'emulation-mode-map-alists 'modal-mode-emulation-alist))
    ;; Clean up when deactivating
    (when (member major-mode modal-mode-high-precedence-modes)
      (setq emulation-mode-map-alists 
            (delq 'modal-mode-emulation-alist emulation-mode-map-alists)))))

;;;###autoload
(define-globalized-minor-mode global-modal-mode
  modal-mode
  modal-mode-maybe-activate
  :group 'modal)

(provide 'modal)

