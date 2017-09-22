(install 'flycheck)

(install 'avy)
(setq avy-all-windows nil
      avy-keys '(?a ?s ?h ?g ?y ?t ?n ?e ?o ?i ?' ?u ?p ?d ?r ?c ?k)
      avy-timeout-seconds 0.2)

(install 'anzu 'require)
(global-anzu-mode +1)

(install 'multiple-cursors)

(defun list-todo-in-current-dir ()
  (interactive)
  (grep "grep -nHsI --color=never -B1 -A2 TODO *")
  (save-excursion
    (with-current-buffer "*grep*"
      (highlight-regexp "\\(TODO\\|FIXME\\)"))))

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-windows.el

(defun split-window-vertically-instead ()
  (interactive)
  (let* ((next-buffer (window-buffer (next-window))))
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer next-buffer)
    (other-window 1)))

(defun split-window-horizontally-instead ()
  (interactive)
  (let* ((next-buffer (window-buffer (next-window))))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (switch-to-buffer next-buffer)
    (other-window 1)))

(defun uuid ()
  (interactive)
  (let* ((raw (with-temp-buffer
		(shell-command "uuidgen" (current-buffer))
		(buffer-substring (point-min) (point-max))))
	 (uuid (progn (string-match "[ \t\n]*$" raw) (replace-match "" nil nil raw))))
    (insert uuid)))

(defun fingers-mode-visual-toggle-enabled-modeline ()
  (let* ((right (format-mode-line "%*  %l,%c"))
         (left (format-mode-line "%b "))
         (available-width (- (window-width) (length left) (length right))))
    (format "%s%s%s" left (make-string available-width ?-) right)))

(defconst fingers-mode-visual-toggle-mode-line mode-line-format)
(defconst fingers-mode-visual-toggle-header-line header-line-format)
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

(defconst fingers-custom-map
  (let* ((m (make-sparse-keymap)))
    (suppress-keymap m t)
    (define-key m (kbd ".") 'ivy-git-ls-files-project)
    (define-key m (kbd "N") 'compilation-goto-next-error)
    (define-key m (kbd "bn") 'rename-buffer)
    (define-key m (kbd "br") 'revert-buffer)
    (define-key m (kbd "bw") 'delete-trailing-whitespace)
    (define-key m (kbd "c") 'count-words)
    (define-key m (kbd "e") 'explode-arguments-into-multiple-lines)
    (define-key m (kbd "m") 'ivy-jump-to-project)
    (define-key m (kbd "n") 'compilation-goto-first-error)
    (define-key m (kbd "o") 'flycheck-next-error)
    (define-key m (kbd "s") 'sort-lines)
    (define-key m (kbd "te") 'enable-all-tests)
    (define-key m (kbd "ti") 'ignore-all-tests)
    (define-key m (kbd "tn") 'scala-next-test-forward)
    (define-key m (kbd "tp") 'scala-next-test-whitespace)
    (define-key m (kbd "u") 'uuid)
    (define-key m (kbd "ve") 'git-gutter:next-hunk)
    (define-key m (kbd "vm") 'git-gutter:mark-hunk)
    (define-key m (kbd "vo") 'git-gutter:previous-hunk)
    (define-key m (kbd "vp") 'git-gutter:popup-hunk)
    (define-key m (kbd "vr") 'git-gutter:revert-hunk)
    (define-key m (kbd "vs") 'git-gutter:stage-hunk)
    (define-key m (kbd "wb") 'balance-windows)
    m))

(defun fingers-mode-custom-bindings ()
  (interactive)
  (after 'dired (define-key dired-mode-map (kbd "C-o") nil))
  (after 'wdired (define-key wdired-mode-map (kbd "C-o") nil))
  (after 'compile (define-key compilation-mode-map (kbd "C-o") nil))
  (define-key global-map (kbd "C-o") 'global-fingers-mode)

  (define-key fingers-mode-map (kbd "<up>") 'windmove-up)
  (define-key fingers-mode-map (kbd "<down>") 'windmove-down)
  (define-key fingers-mode-map (kbd "<right>") 'windmove-right)
  (define-key fingers-mode-map (kbd "<left>") 'windmove-left)

  (define-key fingers-mode-map (kbd "S") 'fingers-end-of-line-and-insert)
  (define-key fingers-mode-map (kbd "A") 'fingers-beginning-of-line-and-insert)

  ;;     j    f    u    p    ;     [    ]
  ;; E/bob   ag   iag  swi  pop  occ< occ>
  ;;     y    n    e    o    i     '
  ;; Bop/bol  <    v    ^    >   Eop/eol
  ;;     k    l    ?    .    /
  ;;   apr    avy      jmp  undo
  (define-key fingers-mode-map (kbd "f") 'ag)
  (define-key fingers-mode-map (kbd "F") 'ag-with-thing-at-point)
  (define-key fingers-mode-map (kbd "u") 'save-buffer)
  (define-key fingers-mode-map (kbd "U") 'recenter-top-bottom)
  (define-key fingers-mode-map (kbd "p") 'swiper-tweaked)
  (define-key fingers-mode-map (kbd "P") 'swiper-with-thing-at-point)
  (define-key fingers-mode-map (kbd "k") 'ivy-apropos)
  (define-key fingers-mode-map (kbd "l") 'avy-goto-char-timer)
  (define-key fingers-mode-map (kbd "L") 'avy-goto-char-in-line)
  (define-key fingers-mode-map (kbd ".") 'ivy-jump)
  (define-key fingers-mode-map (kbd ",") 'pop-to-mark-command)

  ;;     q    d    r    w    b
  ;;  cstm   del in/rp qrp  cpy/slrp
  ;;     a    s    h    t    g
  ;;  encl  spli  ynk  kll  meta
  ;;     z    x    m    c    v
  ;;   zoom  x-  jreg  c-   opn
  (define-key fingers-mode-map (kbd "r") 'fingers-insert-char)
  (define-key fingers-mode-map (kbd "R") 'fingers-replace-with-char)
  (define-key fingers-mode-map (kbd "w") 'anzu-query-replace-regexp)
  (define-key fingers-mode-map (kbd "W") 'anzu-query-replace-at-cursor)
  (define-key fingers-mode-map (kbd "b") 'fingers-slurp-forward)
  (define-key fingers-mode-map (kbd "B") 'fingers-barf-forward)

  (define-key fingers-mode-map (kbd "H") 'counsel-yank-pop)

  (define-key fingers-mode-map (kbd "z") 'toggle-window-zoom)
  (define-key fingers-mode-map (kbd "Z") 'window-configuration-to-register)
  (define-key fingers-mode-map (kbd "M") 'kmacro-to-register)
  (define-key fingers-mode-map (kbd "m") 'jump-to-register)

  (define-key fingers-mode-map (kbd "|") 'mc/edit-lines)
  (define-key fingers-mode-map (kbd "\\") 'goto-line)

  (define-key fingers-mode-c-map (kbd "RET") 'browse-url-at-point)

  (define-key fingers-mode-x-map (kbd "f") 'counsel-find-file)
  (define-key fingers-mode-x-map (kbd "x") 'counsel-M-x)
  (define-key fingers-mode-x-map (kbd "vs") 'show-eshell-git-status)
  (define-key fingers-mode-x-map (kbd "u") 'kmacro-start-macro)
  (define-key fingers-mode-x-map (kbd "U") 'kmacro-end-macro)
  (define-key fingers-mode-x-map (kbd "m") 'create-mail)
  (define-key fingers-mode-x-map (kbd "n") 'kmacro-end-and-call-macro)
  (define-key fingers-mode-x-map (kbd "\\") 'split-window-horizontally-instead)
  (define-key fingers-mode-x-map (kbd "-") 'split-window-vertically-instead)

  (define-key fingers-mode-launch-map (kbd "e") 'last-eshell)
  (define-key fingers-mode-launch-map (kbd "m") 'single-window-magit-status)
  (define-key fingers-mode-launch-map (kbd "n") 'notmuch)
  (define-key fingers-mode-launch-map (kbd "l") 'list-todo-in-current-dir)
  (define-key fingers-mode-launch-map (kbd "oo") 'offlineimap)

  (define-key fingers-mode-toggle-map (kbd "s") 'scala-errors-mode)
  (define-key fingers-mode-toggle-map (kbd "f") 'font-lock-mode)
  (define-key fingers-mode-toggle-map (kbd "w") 'leerzeichen-mode)
  (define-key fingers-mode-toggle-map (kbd "n") 'nlinum-mode)
  (define-key fingers-mode-toggle-map (kbd "z") 'toggle-window-zoom)

  (define-key fingers-mode-map (kbd "M-<up>") 'windmove-up)
  (define-key fingers-mode-map (kbd "M-<down>") 'windmove-down)
  (define-key fingers-mode-map (kbd "M-<right>") 'windmove-right)
  (define-key fingers-mode-map (kbd "M-<left>") 'windmove-left)

  (define-key fingers-mode-map (kbd "S-<up>") 'enlarge-window)
  (define-key fingers-mode-map (kbd "S-<down>") 'shrink-window)
  (define-key fingers-mode-map (kbd "S-<left>") 'shrink-window-horizontally)
  (define-key fingers-mode-map (kbd "S-<right>") 'enlarge-window-horizontally)

  (define-key fingers-mode-map (kbd "q") fingers-custom-map)
  )

(define-key global-map (kbd "M->") 'pop-tag-mark)

(defun compilation-goto-first-error ()
  (interactive)
  (with-current-buffer "*compilation*"
    (goto-char (point-min))
    (compilation-next-error 1)
    (compile-goto-error)))

(defun compilation-goto-next-error ()
  (interactive)
  (with-current-buffer "*compilation*"
    (compilation-next-error 1)
    (compile-goto-error)))

(defun single-window-magit-status ()
  (interactive)
  (magit-status)
  (delete-other-windows))

(defun toggle-window-zoom ()
  (interactive)
  (let ((reg "^"))
    (cond ((and (= (length (window-list)) 1) (get-register reg))
	   (jump-to-register reg)
	   ;; maybe make sure we maintain focus in the same window?
	   )
	  ((> (length (window-list)) 1)
	   (window-configuration-to-register reg)
	   (delete-other-windows)))))

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

(defun find-matching-closer (pos)
  (let ((pair (identify-pair-at pos)))
    (message "identify pair: %s" pair)
    (when pair
      (if (string-equal (car pair) (cdr pair))
	  (find-matching-equal-closer pos pair)
	(find-matching-unequal-closer pos pair)))))

(defun find-matching-equal-closer (pos pair)
  (let ((delimiter (car pair))
	closer-pos)
    (save-excursion
      (goto-char (1+ pos))
      (while (and (not closer-pos) (not (eobp)))
	(cond ((looking-at (regexp-quote (concat "\\" delimiter)))
	       (forward-char 2))
	      ((looking-at (regexp-quote delimiter))
	       (setq closer-pos (point)))
	      (t
	       (forward-char 1)))))
    closer-pos))

(defun find-matching-unequal-closer (pos pair)
  (save-excursion
    (let* ((opener (car pair))
	   (closer (cdr pair))
	   (open-count 1))
      (goto-char (1+ pos))
      (while (and (not (eobp)) (< 0 open-count))
	(cond ((looking-at (regexp-quote opener)) (setq open-count (1+ open-count)))
	      ((looking-at (regexp-quote closer)) (setq open-count (1- open-count))))
	(forward-char 1))
      (unless (eobp) (1- (point))))))

;; TODO: add ` " '
(defun identify-pair-at (pos)
  (save-excursion
    (goto-char pos)
    (cond
	   ((looking-at "(")
	    '("(" . ")"))
	   ((looking-at "{")
	   '("{" . "}"))
	  ((looking-at "\\[")
	   '("[" . "]"))
	  ((looking-at "<")
	   '("<" . ">"))
	  ((looking-at "`")
	   '("`" . "`"))
	  ((looking-at "'")
	   '("'" . "'"))
	  ((looking-at "\"")
	   '("\"" . "\"")))))

(defun find-first-preceding-pair-opener ()
  (save-excursion
    (re-search-backward "(\\|{\\|\\[\\|<\\|\"\\|'")))

(defun surrounding-pair-info ()
  (let* ((start-pos (find-first-preceding-pair-opener))
	 (end-pos (find-matching-closer start-pos))
	 (pair (identify-pair-at start-pos)))
    `((:start-pos . ,start-pos) (:end-pos . ,end-pos) (:pair . ,pair))))

(defun fingers-slurp-forward ()
  (interactive)
  (let* ((info (surrounding-pair-info))
	 (end-pos (cdr (assoc :end-pos info))))
    (if (not (and info end-pos)) (message "couldn't find matching closer, info=%s." info)
      (save-excursion
	(goto-char (cdr (assoc :end-pos info)))
	(delete-char 1)
	(forward-symbol 1)
	(insert (cddr (assoc :pair info)))))))

;; ()abc
;; ()abc def ghi
;; (+ )abc
;; []abc
;; []abc def ghi
;; <html>abc
;; <html> abc def ghi
;; <html a="b"> abc def ghi
;; ""abc
;; "" abc
;; "abc " abc
;; "abc \" " abc
;; 'abc ""\' ' abc

(defun fingers-barf-forward ()
  (interactive)
  (let* ((info (surrounding-pair-info))
	 (end-pos (cdr (assoc :end-pos info))))
    (if (not (and info end-pos)) (message "couldn't find matching closer, info=%s." info)
      (save-excursion
	(goto-char (cdr (assoc :end-pos info)))
	(delete-char 1)
	(forward-symbol -1)
	(insert (cddr (assoc :pair info)))
	))))

;; (abc)
;; (abc def ghi)
;; (+ abc)
;; [abc]
;; [abc def ghi]
;; <html abc def ghi>
;; <html a="b" abc def ghi>
;; "abc"
;; " abc"
;; "abc  abc"
;; "abc \"  abc"
;; 'abc ""\'  abc'

(defun explode-arguments-into-multiple-lines ()
  (interactive)
  (let ((info (surrounding-pair-info)))
    (when info
      (goto-char (cdr (assoc :end-pos info)))
      (open-line 1)
      (while (> (1- (point)) (cdr (assoc :start-pos info)))
	(forward-char -1)
	(when (looking-at ",") (forward-char 1) (open-line 1) (forward-char -1)))
      (open-line 1)
      (set-mark (point))
      (goto-char (1+ (find-matching-closer (cdr (assoc :start-pos info)))))
      (indent-for-tab-command)
      (goto-char (cdr (assoc :start-pos info))))))

;; func(a,b,c)

(add-hook 'fingers-after-reset-hook 'fingers-mode-custom-bindings)
(install 'fingers)
(global-fingers-mode 1)

(after 'dired
  (define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode))
(after 'diff-mode
  (dolist (key '("n" "N" "p" "P" "k" "K" "W" "o" "A" "r" "R"))
    (define-key diff-mode-shared-map (kbd key) nil)))
