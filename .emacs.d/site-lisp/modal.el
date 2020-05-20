;; -*- lexical-binding: t -*-

(defun modal-mode-visual-toggle ()
  (interactive)
  (let ((faces-to-toggle '(highlight)))
    (cond (modal-mode
           (mapcar (lambda (face)
                     (set-face-background face "#f5f5f5"))
                   faces-to-toggle))
          (t
           (mapcar (lambda (face)
                     (set-face-background face "#FFD54F"))
                   faces-to-toggle)))))

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
  (modal-mode -1))

;;;###autoload
(define-minor-mode modal-mode
  "Minor mode that installs a top level command keymap that allows for modal editing.

Available bindings:

\\{modal-mode-map}
"
  nil " modal" modal-mode-map :group 'modal)

;;;###autoload
(define-globalized-minor-mode global-modal-mode
  modal-mode
  modal-mode-maybe-activate
  :group 'modal)

(add-hook 'modal-mode-hook 'modal-mode-visual-toggle)

(provide 'modal)
