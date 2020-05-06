;; -*- lexical-binding: t -*-

(defconst modal-modeline-active-fg "#263238")
(defconst modal-modeline-active-bg "#cfd8dc")
(defconst modal-modeline-inactive-fg "#263238")
(defconst modal-modeline-inactive-bg "#8BC34A")

(defun modal-mode-visual-toggle ()
  (interactive)
  (let ((faces-to-toggle '(mode-line mode-line-inactive)))
    (cond (modal-mode
           (mapcar (lambda (face)
                     (set-face-foreground face modal-modeline-active-fg)
                     (set-face-background face modal-modeline-active-bg))
                   faces-to-toggle))
          (t
           (mapcar (lambda (face)
                     (set-face-background face modal-modeline-inactive-bg)
                     (set-face-foreground face modal-modeline-inactive-fg))
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
