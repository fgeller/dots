(use-package magit
  :ensure magit
  :config (setq git-commit-summary-max-length 72))

(use-package magit-gh-pulls
  :init (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(setq magit-display-buffer-function
      (lambda (b)
        (display-buffer b '(display-buffer-same-window))))

(use-package multiple-cursors
  :ensure multiple-cursors
  :commands mc/edit-lines)

(use-package avy
  :ensure avy
  :commands (avy-goto-word-or-subword-1)
  :config (setq avy-all-windows nil
                avy-keys '(?a ?s ?h ?t ?n ?e ?o ?i)))

(use-package anzu
  :ensure anzu
  :config (global-anzu-mode +1))

(defconst fingers-mode-visual-toggle-mode-line nil)
(defconst fingers-mode-visual-toggle-header-line nil)

(defun fingers-mode-visual-toggle-enabled-modeline ()
  (let* ((right (format-mode-line "%*  %l,%c"))
         (left (format-mode-line "%b "))
         (available-width (- (window-width) (length left) (length right))))
    (format "%s%s%s" left (make-string available-width ?-) right)))

(setq fingers-mode-visual-toggle-mode-line mode-line-format)
(setq fingers-mode-visual-toggle-header-line header-line-format)
(setq-default mode-line-format '((:eval (fingers-mode-visual-toggle-enabled-modeline))))

(defun fingers-mode-visual-toggle ()
  (interactive)
  (let ((faces-to-toggle '(mode-line mode-line-inactive)))
    (cond (fingers-mode
           (mapcar (lambda (face)
                     (set-face-foreground face "black")
                     (set-face-background face "white")
                     (set-face-attribute face nil :height (face-attribute 'default :height)))
                   faces-to-toggle)
           (setq header-line-format nil))
          (t
           (mapcar (lambda (face)
                     (set-face-background face (if (window-system) "#66BB6A" "green"))
                     (set-face-foreground face "white")
                     (set-face-attribute face nil :height (face-attribute 'default :height)))
                   faces-to-toggle)
           ))))

(add-hook 'fingers-mode-hook 'fingers-mode-visual-toggle)

(defun offlineimap ()
  "Helper to (re)start offlineimap via compile"
  (interactive)
  (let ((buf "*offline-imap*"))
    (if (get-buffer buf)
        (with-current-buffer buf (recompile))
      (compile "offlineimap")
      (with-current-buffer "*compilation*"
        (rename-buffer "*offline-imap*")))))

(defun touch-buffer ()
  (interactive "")
  (insert " ")
  (backward-delete-char 1)
  (save-buffer))

(defun toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(defun ag-project-with-thing-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (ag-project thing)))

(defun fingers-mode-custom-bindings ()
  (interactive)
  (define-key dired-mode-map (kbd "C-o") nil)
  (eval-after-load 'wdired '(define-key wdired-mode-map (kbd "C-o") nil))
  (eval-after-load 'compile '(define-key compilation-mode-map (kbd "C-o") nil))
  (define-key global-map (kbd "C-o") 'global-fingers-mode)

  ;;     j    f    u    p    ;     [    ]

  ;; E/bob   ag   iag  swi  pop  occ< occ>

  ;;     y    n    e    o    i     '

  ;; Bop/bol  <    v    ^    >   Eop/eol

  ;;     k    l    ?    .    /

  ;;   apr    avy      jmp  undo

  (define-key fingers-mode-map (kbd "H") 'counsel-yank-pop)
  (define-key fingers-mode-map (kbd "k") 'ivy-apropos)
  (define-key fingers-mode-map (kbd "l") 'avy-goto-char)
  (define-key fingers-mode-map (kbd "L") 'avy-goto-char-in-line)

  (define-key fingers-mode-map (kbd "b") 'fingers-end-of-line-and-insert)
  (define-key fingers-mode-map (kbd "B") 'fingers-beginning-of-line-and-insert)

  (define-key fingers-mode-map (kbd "f") 'ag-project)
  (define-key fingers-mode-map (kbd "F") 'ag-project-with-thing-at-point)
  (define-key fingers-mode-map (kbd "u") 'ivy-ag)
  (define-key fingers-mode-map (kbd "U") 'ivy-ag-with-thing-at-point)
  (define-key fingers-mode-map (kbd "p") (fingers-nav-command swiper-tweaked))
  (define-key fingers-mode-map (kbd "P") (fingers-nav-command swiper-with-thing-at-point))

  (define-key fingers-mode-map (kbd "X") 'anzu-query-replace)

  (define-key fingers-mode-map (kbd "z") 'ivy-resume)
  (define-key fingers-mode-map (kbd ".") 'ivy-jump)
  (define-key fingers-mode-map (kbd "|") 'mc/edit-lines)

  (define-key fingers-mode-c-map (kbd "RET") 'browse-url-at-point)

  (define-key fingers-mode-x-map (kbd "f") 'counsel-find-file)
  (define-key fingers-mode-x-map (kbd "x") 'counsel-M-x)
  (define-key fingers-mode-x-map (kbd "vs") 'show-eshell-git-status)

  (define-key fingers-mode-launch-map (kbd "e") 'last-eshell)
  (define-key fingers-mode-launch-map (kbd "s") 'shell)
  (define-key fingers-mode-launch-map (kbd "m") 'magit-status)
  (define-key fingers-mode-launch-map (kbd "n") 'notmuch)
  (define-key fingers-mode-launch-map (kbd "oo") 'offlineimap)

  (define-key fingers-mode-toggle-map (kbd "s") 'scala-errors-mode)
  (define-key fingers-mode-toggle-map (kbd "f") 'font-lock-mode)
  (define-key fingers-mode-toggle-map (kbd "w") 'leerzeichen-mode)
  (define-key fingers-mode-toggle-map (kbd "W") 'toggle-show-trailing-whitespace)
  (define-key fingers-mode-toggle-map (kbd "n") 'nlinum-mode)

  (define-key fingers-mode-map (kbd "A-<up>") 'increase-font-height)
  (define-key fingers-mode-map (kbd "s-<up>") 'increase-font-height)
  (define-key fingers-mode-map (kbd "A-<down>") 'decrease-font-height)
  (define-key fingers-mode-map (kbd "s-<down>") 'decrease-font-height)

  (define-key fingers-mode-map (kbd "S-<up>") 'enlarge-window)
  (define-key fingers-mode-map (kbd "S-<down>") 'shrink-window)
  (define-key fingers-mode-map (kbd "S-<left>") 'shrink-window-horizontally)
  (define-key fingers-mode-map (kbd "S-<right>") 'enlarge-window-horizontally)

  (let* ((my-fingers-map (fingers-mode-clean-map)))
    (define-key my-fingers-map (kbd "m") 'ivy-jump-to-project)
    (define-key my-fingers-map (kbd ".") 'ivy-git-ls-files-project)
    (define-key my-fingers-map (kbd "ti") 'ignore-all-tests)
    (define-key my-fingers-map (kbd "te") 'enable-all-tests)
    (define-key my-fingers-map (kbd "tn") 'scala-next-test-forward)
    (define-key my-fingers-map (kbd "tp") 'scala-next-test-whitespace)
    (define-key my-fingers-map (kbd "nn") 'scala-errors-goto-first-error)
    (define-key my-fingers-map (kbd "ne") 'scala-errors-goto-next-error)
    (define-key my-fingers-map (kbd "no") 'scala-errors-goto-prev-error)
    (define-key my-fingers-map (kbd "N") 'js2-next-error)
    (define-key my-fingers-map (kbd "br") 'revert-buffer)
    (define-key my-fingers-map (kbd "bn") 'rename-buffer)
    (define-key my-fingers-map (kbd "bt") 'touch-buffer)
    (define-key my-fingers-map (kbd "bw") 'delete-trailing-whitespace)
    (define-key my-fingers-map (kbd "e") 'explode-arguments-into-multiple-lines)
    (define-key my-fingers-map (kbd "p") 'ivy-ag-with-thing-at-point-in-main)
    (define-key my-fingers-map (kbd "s") 'sort-lines)
    (define-key fingers-mode-map (kbd "q") my-fingers-map))
  )

(defun go-ignore-all-tests ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp
     "func Test\\([^(]+\\)("
     "func IgnoreTest\\1("
     nil
     (point-min)
     (point-max))))

(defun go-enable-all-tests ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp
     "func IgnoreTest\\([^(]+\\)("
     "func Test\\1("
     nil
     (point-min)
     (point-max))))


(defun ignore-all-tests ()
  (interactive)
  (cond ((eq major-mode 'go-mode)
         (call-interactively 'go-ignore-all-tests))
        ((eq major-mode 'scala-mode)
         (call-interactively 'scala-ignore-all-tests))))

(defun enable-all-tests ()
  (interactive)
  (cond ((eq major-mode 'go-mode)
         (call-interactively 'go-enable-all-tests))
        ((eq major-mode 'scala-mode)
         (call-interactively 'scala-enable-all-tests))))

(defun find-matching-closer (pair)
  (let* ((start-pos (point))
         (open-count 1))
    (while (and (not (eobp))
                (< 0 open-count))
      (forward-char 1)
      (cond ((looking-at (car pair)) (setq open-count (1+ open-count)))
            ((looking-at (cdr pair)) (setq open-count (1- open-count)))))
    (unless (eobp) (point))))

(defun explode-arguments-into-multiple-lines ()
  (interactive)
  (let* ((start-pos (1+ (re-search-backward "(\\|{\\|\\[")))
         (pair (save-excursion
                 (goto-char start-pos)
                 (cond ((looking-at "{") '("(" .")"))
                       ((looking-at "\\[") '("[" . "]"))
                       (t '("(" . ")")))))
         (end-pos (save-excursion
                    (goto-char start-pos)
                    (find-matching-closer pair))))
    (goto-char end-pos)
    (open-line 1)
    (while (> (point) start-pos)
      (forward-char -1)
      (when (looking-at ",") (forward-char 1) (open-line 1) (forward-char -1)))
    (open-line 1)
    (set-mark (point))
    (goto-char (1+ (find-matching-closer pair)))
    (indent-for-tab-command)
    (goto-char start-pos)))

(eval-after-load 'fingers
  'nil)

(use-package fingers
  :commands global-fingers-mode
  :init
  (add-hook 'fingers-after-reset-hook 'fingers-mode-custom-bindings)
  (global-fingers-mode 1))

(defun save-macro-to-scratch (name)
  (interactive "SName the macro:")
  (name-last-kbd-macro name)
  (pop-to-buffer "*scratch*")
  (goto-char (point-max))
  (newline)
  (insert ";; generated macro")
  (newline)
  (insert-kbd-macro name)
  (newline))

(define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode)

(eval-after-load 'diff-mode
  '(progn
     (dolist (key '("n" "N" "p" "P" "k" "K" "W" "o" "A" "r" "R"))
       (define-key diff-mode-shared-map (kbd key) nil))))
