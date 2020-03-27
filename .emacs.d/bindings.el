;; TODO expand/contract - region?
;; TODO review specifiers
;; TODO ivy jump projects?

;;  -------------  ---------------  --------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  --------------------------
;; |             ||               ||              ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                          |
;; |     `       ||       1       ||       2      ||       3       ||       4       ||       5       ||       6       ||       7       ||       8       ||       9       ||       0       ||       -       ||       =       ||        backspace         |
;; |counsel-unic ||               ||              ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                          |
;;  -------------  ---------------  --------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  --------------------------

;;  --------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------
;; |                    ||               ||  delete-back  || duplicate-lin ||   join-line   ||     barf      ||               ||               ||               ||   swiper-all  || pop-to-mark   || prev-word-occ || next-word-occ ||                  |
;; |       tab          ||       q       ||       d       ||       r       ||       w       ||       b       ||       j       ||       f       ||       u       ||       p       ||       ;       ||       [       ||       ]       ||        \         |
;; |                    ||               ||     delete    || query-repl-rx ||   open-line   ||     slurp     ||               ||               || avy-goto-char ||     swiper    || ivy-mark-ring ||  prev-sym-occ || next-sym-occ  ||  goto-line       |
;;  --------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------

;;  ------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  -------------------------------
;; |                        ||    insert     ||  surround     ||  counsel-yank ||               ||               ||  beg-buffer   ||  back-symbl   ||   page-down   ||    page-up    ||   fwd-symbl   ||  end-buffer   ||                               |
;; |         control        ||       a       ||       s       ||       h       ||       t       ||       g       ||       y       ||       n       ||       e       ||       o       ||       i       ||       '       ||              return           |
;; |                        ||     replace   || rem-encl-pair ||      yank     ||     kill      ||  insert-mode  || begining-line ||     left      ||     down      ||      up       ||     right     ||   end-line    ||                               |
;;  ------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  -------------------------------

;;  ------------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------------------------------
;; |                              ||  ivy-resume   ||               ||               ||               ||               ||               ||               ||dumb-jump-back || xref-jump-back||               ||                                          |
;; |           shift              ||       z       ||       x       ||       m       ||       c       ||       v       ||       k       ||       l       ||       ,       ||       .       ||       /       ||                shift                     |
;; |                              ||   win-zoom    ||     x-map     ||  macro-map    ||     c-map     || save-buffer   || counsel-open  ||  counsel-rg   || dumb-jump-go  || xref-jump-to  ||      undo     ||                                          |
;;  ------------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------------------------------

(install 'modal)
(global-modal-mode 1)
(after 'dired (define-key dired-mode-map (kbd "C-o") nil))
(after 'wdired (define-key wdired-mode-map (kbd "C-o") nil))
(after 'compile (define-key compilation-mode-map (kbd "C-o") nil))
(define-key global-map (kbd "C-o") 'global-modal-mode)

(install 'visual-regexp)
(install 'dumb-jump)
(install 'nlinum)
(install 'move-border)

(install 'avy)
(setq avy-all-windows nil
      avy-keys '(?a ?s ?h ?g ?y ?t ?n ?e ?o ?i ?' ?u ?p ?d ?r ?c ?k)
      avy-timeout-seconds 0.2)

;; c-map
(defconst c-bindings-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)

    (define-key map (kbd "cc") 'with-editor-finish)
    (define-key map (kbd "cm") 'recompile)
    (define-key map (kbd "cM") 'compile)
    (define-key map (kbd ";") 'comment-dwim)

    (define-key map (kbd "ll") 'lsp)
    (define-key map (kbd "lr") 'lsp-rename)
    (define-key map (kbd "la") 'xref-find-apropos)

    (define-key map (kbd "q") 'fill-paragraph)

    map))

;; x-map
(defconst x-bindings-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)

    (define-key map (kbd "-") 'split-window-vertically-instead)
    (define-key map (kbd "\\") 'split-window-horizontally-instead)

    (define-key map (kbd "a") 'counsel-apropos)

    (define-key map (kbd "bb") 'switch-to-buffer)
    (define-key map (kbd "bR") 'rename-buffer)
    (define-key map (kbd "br") 'revert-buffer)
    (define-key map (kbd "bw") 'delete-trailing-whitespace)

    (define-key map (kbd "c") 'save-buffers-kill-terminal)
    (define-key map (kbd "e") 'eval-last-sexp)
    (define-key map (kbd "f") 'counsel-find-file)
    (define-key map (kbd "h") 'mark-whole-buffer)
    (define-key map (kbd "k") 'kill-buffer)
    (define-key map (kbd "m") 'magit-status)
    (define-key map (kbd "o") 'other-window)
    (define-key map (kbd "r") 'eval-region)
    (define-key map (kbd "s") 'save-buffer)
    (define-key map (kbd "S") 'save-some-buffer)

    (define-key map (kbd "td") 'toggle-debug-on-error)
    (define-key map (kbd "tf") 'font-lock-mode)
    (define-key map (kbd "tl") 'toggle-truncate-lines)
    (define-key map (kbd "tw") 'leerzeichen-mode)

    (define-key map (kbd "ve") 'git-gutter:next-hunk)
    (define-key map (kbd "vo") 'git-gutter:previous-hunk)
    (define-key map (kbd "vp") 'git-gutter:popup-hunk)
    (define-key map (kbd "vr") 'git-gutter:revert-hunk)
    (define-key map (kbd "v<SPC>") 'git-gutter:mark-hunk)

    (define-key map (kbd "x") 'counsel-M-x)

    (define-key map (kbd "0") 'delete-window)
    (define-key map (kbd "1") 'delete-other-windows)
    (define-key map (kbd "2") 'split-window-below)
    (define-key map (kbd "3") 'split-window-right)

    map))

;; macro map - cf `kmacro-keymap'
(defconst macro-bindings-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "m") 'kmacro-start-macro)
    (define-key map (kbd "M") 'kmacro-end-macro)
    (define-key map (kbd "<SPC>") 'apply-macro-to-region-lines)
    (define-key map (kbd "e") 'kmacro-end-and-call-macro)
    map))

(define-key modal-mode-map (kbd "M-`") 'counsel-unicode-char)
(define-key modal-mode-map (kbd "`") 'counsel-unicode-char)
(define-key modal-mode-map (kbd "<SPC>") 'mark-select)
(define-key modal-mode-map (kbd "+") 'increment-integer-at-point)
(define-key modal-mode-map (kbd "-") 'decrement-integer-at-point)

(define-key modal-mode-map (kbd "q") nil)
(define-key modal-mode-map (kbd "Q") nil)
(define-key modal-mode-map (kbd "d") 'delete-forward-char)
(define-key modal-mode-map (kbd "D") 'delete-backward-char)
(define-key modal-mode-map (kbd "r") 'vr/query-replace)
(define-key modal-mode-map (kbd "R") 'duplicate-line)
(define-key modal-mode-map (kbd "w") 'open-line-below)
(define-key modal-mode-map (kbd "W") 'join-line)
(define-key modal-mode-map (kbd "b") 'slurp-forward)
(define-key modal-mode-map (kbd "B") 'barf-forward)
(define-key modal-mode-map (kbd "j") nil)
(define-key modal-mode-map (kbd "J") nil)
(define-key modal-mode-map (kbd "f") nil)
(define-key modal-mode-map (kbd "F") nil)
(define-key modal-mode-map (kbd "u") 'avy-goto-char-timer)
(define-key modal-mode-map (kbd "U") nil)
(define-key modal-mode-map (kbd "p") 'swiper)
(define-key modal-mode-map (kbd "P") 'swiper-all)
(define-key modal-mode-map (kbd ";") 'ivy-mark-ring)
(define-key modal-mode-map (kbd ":") 'pop-to-mark-command)
(define-key modal-mode-map (kbd "[") 'move-to-previous-symbol-occurrence)
(define-key modal-mode-map (kbd "{") 'move-to-previous-word-occurrence)
(define-key modal-mode-map (kbd "]") 'move-to-next-symbol-occurrence)
(define-key modal-mode-map (kbd "}") 'move-to-next-word-occurrence)
(define-key modal-mode-map (kbd "\\") 'goto-line)
(define-key modal-mode-map (kbd "|") nil)

(define-key modal-mode-map (kbd "a") 'replace-select)
(define-key modal-mode-map (kbd "A") 'insert-literal)
(define-key modal-mode-map (kbd "s") 'remove-enclosing-pair)
(define-key modal-mode-map (kbd "S") 'enclose-in-pair)
(define-key modal-mode-map (kbd "h") 'yank)
(define-key modal-mode-map (kbd "H") 'counsel-yank-pop)
(define-key modal-mode-map (kbd "t") 'kill-select)
(define-key modal-mode-map (kbd "T") nil)
(define-key modal-mode-map (kbd "g") 'modal-mode-deactivate)
(define-key modal-mode-map (kbd "G") nil)
(define-key modal-mode-map (kbd "y") 'beginning-of-line)
(define-key modal-mode-map (kbd "Y") 'beginning-of-buffer)
(define-key modal-mode-map (kbd "n") 'left-char)
(define-key modal-mode-map (kbd "N") 'backward-word)
(define-key modal-mode-map (kbd "e") 'next-line)
(define-key modal-mode-map (kbd "E") 'scroll-up-half-page)
(define-key modal-mode-map (kbd "o") 'previous-line)
(define-key modal-mode-map (kbd "O") 'scroll-down-half-page)
(define-key modal-mode-map (kbd "i") 'right-char)
(define-key modal-mode-map (kbd "I") 'forward-word)
(define-key modal-mode-map (kbd "'") 'end-of-line)
(define-key modal-mode-map (kbd "\"") 'end-of-buffer)

(define-key modal-mode-map (kbd "z") 'toggle-window-zoom)
(define-key modal-mode-map (kbd "Z") 'ivy-resume)
(define-key modal-mode-map (kbd "x") x-bindings-map)
(define-key modal-mode-map (kbd "X") nil)
(define-key modal-mode-map (kbd "m") macro-bindings-map)
(define-key modal-mode-map (kbd "M") nil)
(define-key modal-mode-map (kbd "c") c-bindings-map)
(define-key modal-mode-map (kbd "C") nil)
(define-key modal-mode-map (kbd "v") 'save-buffer)
(define-key modal-mode-map (kbd "V") nil)
(define-key modal-mode-map (kbd "k") 'ivy-jump)
(define-key modal-mode-map (kbd "K") 'ivy-git-ls-files-project)
(define-key modal-mode-map (kbd "l") 'counsel-rg)
(define-key modal-mode-map (kbd "L") nil)
(define-key modal-mode-map (kbd ",") 'dumb-jump-go)
(define-key modal-mode-map (kbd "<") 'dumb-jump-back)
(define-key modal-mode-map (kbd ".") 'xref-find-definitions)
(define-key modal-mode-map (kbd ">") 'xref-pop-marker-stack)
(define-key modal-mode-map (kbd "/") 'undo)
(define-key modal-mode-map (kbd "?") nil)

(defun scroll-down-half-page ()
  (interactive)
  (scroll-down-command (/ (window-height) 4))
  (pulse-momentary-highlight-one-line (point)))

(defun scroll-up-half-page ()
  (interactive)
  (scroll-up-command (/ (window-height) 4))
  (pulse-momentary-highlight-one-line (point)))


(defun insert-literal ()
  (interactive)
  (insert (read-string "Insert: ")))

(defun duplicate-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (kill-ring-save (point) (mark))
    (open-line 1)
    (forward-char 1)
    (yank)))

(defun open-line-below ()
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)
    (forward-line 1)
    (indent-for-tab-command)))

(defun find-first-preceding-pair-opener ()
  (save-excursion
    (re-search-backward "(\\|{\\|\\[\\|<\\|\"\\|'")))

(defun find-matching-closer (pos)
  (let ((pair (identify-pair-at pos)))
    (when pair
      (if (string-equal (car pair) (cdr pair))
	  (find-matching-equal-closer pos pair)
	(find-matching-unequal-closer pos pair)))))

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

(defun surrounding-pair-info ()
  (let* ((start-pos (find-first-preceding-pair-opener))
	 (end-pos (find-matching-closer start-pos))
	 (pair (identify-pair-at start-pos)))
    `((:start-pos . ,start-pos) (:end-pos . ,end-pos) (:pair . ,pair))))

(defun slurp-forward ()
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

(defun barf-forward ()
  (interactive)
  (let* ((info (surrounding-pair-info))
	 (end-pos (cdr (assoc :end-pos info))))
    (if (not (and info end-pos)) (message "couldn't find matching closer, info=%s." info)
      (save-excursion
	(goto-char (cdr (assoc :end-pos info)))
	(delete-char 1)
	(forward-symbol -1)
	(insert (cddr (assoc :pair info)))))))

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

(defun beginning-of-symbol ()
  (while (not (looking-at-symbol-p))
    (left-char 1)))

(defun looking-at-symbol-p ()
  (looking-at "\\_<"))

(defun looking-at-word-p ()
  (if subword-mode
      (or (looking-at "\\b")
          (let ((pos-start (point)) pos-after)
            (save-excursion
              (subword-forward -1)
              (subword-forward 1)
              (setq pos-after (point)))
            (= pos-start pos-after)))
    (looking-at "\\b")))

(defun beginning-of-word ()
  (while (not (looking-at-word-p))
    (left-char 1)))

(defun move-to-next-symbol-occurrence ()
  (interactive)
  (beginning-of-symbol)
  (forward-symbol 1)
  (let ((thing (thing-at-point 'symbol)))
    (setq isearch-string thing)
    (search-forward-regexp (concat "\\_<" (regexp-quote thing) "\\_>")))
  (beginning-of-symbol)
  (save-excursion
    (forward-symbol 1)
    (set-mark (point))))

(defun move-to-previous-symbol-occurrence ()
  (interactive)
  (beginning-of-symbol)
  (let ((thing (thing-at-point 'symbol)))
    (setq isearch-string thing)
    (search-backward-regexp (concat "\\_<" (regexp-quote thing) "\\_>"))
    (save-excursion
      (forward-symbol 1)
      (set-mark (point)))))

(defun move-to-next-word-occurrence ()
  (interactive)
  (beginning-of-word)
  (forward-word)
  (let ((thing (thing-at-point 'word)))
    (setq isearch-string thing)
    (search-forward-regexp (concat "\\<" (regexp-quote thing) "\\>")))
  (beginning-of-word)
  (save-excursion
    (forward-word)
    (set-mark (point))))

(defun move-to-previous-word-occurrence ()
  (interactive)
  (beginning-of-word)
  (let ((thing (thing-at-point 'word)))
    (setq isearch-string thing)
    (search-backward-regexp (concat "\\<" (regexp-quote thing) "\\>"))
    (save-excursion
      (forward-word)
      (set-mark (point)))))

(defconst region-specifiers
  '((char . ?v)
    (char-and-whitespace . ?V)
    (line . ?y)
    (line-rest . ?g)
    (word . ?h)
    (word-and-whitespace . ?H)
    (symbol . ?t)
    (symbol-and-whitespace . ?T)
    (between-whitespace . ?c)
    (with-surrounding-whitespace . ?C)
    (inside-pair . ?s)
    (with-pair . ?a)
    (with-pair-and-whitespace . ?A))
  "Mapping from region type to identifier key")

(defun region-specifier (type)
  (cdr (assoc type region-specifiers)))

(defun kill-select ()
  (interactive)
  (cond ((region-active-p) (kill-region (point) (mark)))
	(t (let ((nk (read-char "Kill: ")))
	     (cond
	      ((= nk (region-specifier 'char)) (kill-char))
	      ((= nk (region-specifier 'char-and-whitespace)) (kill-char-and-whitespace))
	      ((= nk (region-specifier 'line)) (kill-whole-line))
	      ((= nk (region-specifier 'line-rest)) (kill-until-end-of-line))
	      ((= nk (region-specifier 'word)) (kill-word))
	      ((= nk (region-specifier 'word-and-whitespace)) (kill-word-and-whitespace))
	      ((= nk (region-specifier 'symbol)) (kill-symbol))
	      ((= nk (region-specifier 'symbol-and-whitespace)) (kill-symbol-and-whitespace))
	      ((= nk (region-specifier 'between-whitespace)) (kill-between-whitespace))
	      ((= nk (region-specifier 'with-surrounding-whitespace)) (kill-with-surrounding-whitespace))
	      ((= nk (region-specifier 'inside-pair)) (kill-inside-pair))
	      ((= nk (region-specifier 'with-pair)) (kill-with-pair))
	      ((= nk (region-specifier 'with-pair-and-whitespace)) (kill-with-pair-and-whitespace))
	      (t (set-mark (point))
		 (call-interactively (key-binding (kbd (string nk))))
		 (kill-region (point) (mark))))))))

(defun kill-char ()
  (mark-char)
  (kill-region (point) (mark)))

(defun kill-char-and-whitespace ()
  (mark-char-and-whitespace)
  (kill-region (point) (mark)))

(defun kill-word ()
  (mark-word)
  (kill-region (point) (mark)))

(defun kill-word-and-whitespace ()
  (mark-word-and-whitespace)
  (kill-region (point) (mark)))

(defun kill-symbol ()
  (mark-symbol)
  (kill-region (point) (mark)))

(defun kill-symbol-and-whitespace ()
  (mark-symbol-and-whitespace)
  (kill-region (point) (mark)))

(defun kill-between-whitespace ()
  (mark-between-whitespace)
  (kill-region (point) (mark)))

(defun kill-with-surrounding-whitespace ()
  (mark-with-surrounding-whitespace)
  (kill-region (point) (mark)))

(defun kill-until-end-of-line ()
  (mark-until-end-of-line)
  (kill-region (point) (mark)))

(defun kill-whole-line ()
  (mark-whole-line)
  (kill-region (point) (mark))
  (delete-char 1))

(defun kill-inside-pair ()
  (mark-inside-pair)
  (kill-region (point) (mark)))

(defun kill-with-pair ()
  (mark-with-pair)
  (kill-region (point) (mark)))

(defun kill-with-pair-and-whitespace ()
  (mark-with-pair-and-whitespace)
  (kill-region (point) (mark)))

(defun pass-events (kbd-string)
  "Helper to pass keyboard events through to shadowed maps. Based on `boon-push-events'"
  (setq unread-command-events
        (append (kbd kbd-string) unread-command-events)))

(defun replace-select ()
  (interactive)
  (unless (region-active-p)
    (let ((nk (read-char "Mark: ")))
      (cond
       ((= nk (region-specifier 'char)) (mark-char))
       ((= nk (region-specifier 'char-and-whitespace)) (mark-char-and-whitespace))
       ((= nk (region-specifier 'line)) (mark-whole-line))
       ((= nk (region-specifier 'line-rest)) (mark-until-end-of-line))
       ((= nk (region-specifier 'word)) (mark-word))
       ((= nk (region-specifier 'word-and-whitespace)) (mark-word-and-whitespace))
       ((= nk (region-specifier 'symbol)) (mark-symbol))
       ((= nk (region-specifier 'symbol-and-whitespace)) (mark-symbol-and-whitespace))
       ((= nk (region-specifier 'between-whitespace)) (mark-between-whitespace))
       ((= nk (region-specifier 'with-surrounding-whitespace)) (mark-with-surrounding-whitespace))
       ((= nk (region-specifier 'inside-pair)) (mark-inside-pair))
       ((= nk (region-specifier 'with-pair)) (mark-with-pair))
       ((= nk (region-specifier 'with-pair-and-whitespace)) (mark-with-pair-and-whitespace))
       (t (mark-word))))) ; defaults to word
  (let ((rp (read-string "Replace with: " (buffer-substring (point) (mark)))))
    (delete-region (point) (mark))
    (insert rp)))

(defun mark-select ()
  (interactive)
  (if (region-active-p) (kill-ring-save nil nil 'region)
    (let ((nk (read-char "Mark: ")))
      (cond
       ((= nk (region-specifier 'char)) (mark-char))
       ((= nk (region-specifier 'char-and-whitespace)) (mark-char-and-whitespace))
       ((= nk (region-specifier 'line)) (mark-whole-line))
       ((= nk (region-specifier 'line-rest)) (mark-until-end-of-line))
       ((= nk (region-specifier 'word)) (mark-word))
       ((= nk (region-specifier 'word-and-whitespace)) (mark-word-and-whitespace))
       ((= nk (region-specifier 'symbol)) (mark-symbol))
       ((= nk (region-specifier 'symbol-and-whitespace)) (mark-symbol-and-whitespace))
       ((= nk (region-specifier 'between-whitespace)) (mark-between-whitespace))
       ((= nk (region-specifier 'with-surrounding-whitespace)) (mark-with-surrounding-whitespace))
       ((= nk (region-specifier 'inside-pair)) (mark-inside-pair))
       ((= nk (region-specifier 'with-pair)) (mark-with-pair))
       ((= nk (region-specifier 'with-pair-and-whitespace)) (mark-with-pair-and-whitespace))
       (t (set-mark (point))
	  (pass-events (string nk)))))))

(defun mark-char ()
  (set-mark (point))
  (forward-char 1))

(defun mark-char-and-whitespace ()
  (set-mark-before-whitespace-and-return)
  (forward-char 1)
  (skip-whitespace-forward))

(defun mark-word ()
  (unless (looking-at-word-p) (beginning-of-word))
  (set-mark (point))
  (forward-word))

(defun mark-word-and-whitespace ()
  (unless (looking-at-word-p) (beginning-of-word))
  (set-mark-before-whitespace-and-return)
  (forward-word)
  (skip-whitespace-forward))

(defun mark-symbol ()
  (unless (looking-at-symbol-p) (beginning-of-symbol))
  (set-mark (point))
  (forward-symbol 1))

(defun mark-symbol-and-whitespace ()
  (unless (looking-at-symbol-p) (beginning-of-symbol))
  (set-mark-before-whitespace-and-return)
  (forward-symbol 1)
  (skip-whitespace-forward))

(defun mark-between-whitespace ()
  (search-backward-regexp "[ \t\n]" (point-min) t)
  (skip-whitespace-forward)
  (set-mark (point))
  (search-forward-regexp "[ \t\n]" (point-max) t)
  (skip-whitespace-backward))

(defun mark-with-surrounding-whitespace ()
  (search-backward-regexp "[ \t\n]" (point-min) t)
  (let ((non-whitespace-position (save-excursion
				   (skip-whitespace-forward)
				   (point))))
    (skip-whitespace-backward)
    (set-mark (point))
    (goto-char non-whitespace-position)
    (search-forward-regexp "[ \t\n]" (point-max) t)
    (skip-whitespace-forward)))

(defun mark-until-end-of-line ()
  (set-mark (point))
  (end-of-line))

(defun mark-whole-line ()
  (beginning-of-line)
  (set-mark (point))
  (end-of-line))

(defun mark-inside-pair ()
  (dispatch-with-pair 'mark-inside-pair-strings
                      (lambda () (set-mark (point)))))

(defun mark-inside-pair-strings (start end)
  (move-point-to-pair-starting-string start end)
  (forward-char 1)
  (set-mark (point))
  (backward-char 1)
  (move-point-to-pair-ending-string start end))

(defun mark-with-pair ()
  (dispatch-with-pair 'mark-with-pair-strings))

(defun mark-with-pair-strings (start end)
  (move-point-to-pair-starting-string start end)
  (set-mark (point))
  (move-point-to-pair-ending-string start end)
  (forward-char 1))

(defun mark-with-pair-strings-and-whitespace (start end)
  (move-point-to-pair-starting-string start end)
  (let ((starting-position (point)))
    (skip-whitespace-backward)
    (set-mark (point))
    (goto-char starting-position))
  (move-point-to-pair-ending-string start end)
  (forward-char 1)
  (skip-whitespace-forward))

(defun mark-with-pair-and-whitespace ()
  (dispatch-with-pair 'mark-with-pair-strings-and-whitespace))

(defun set-mark-before-whitespace-and-return ()
  (let ((start-position (point)))
    (skip-whitespace-backward)
    (set-mark (point))
    (goto-char start-position)))

(defun skip-whitespace-forward ()
  (skip-chars-forward " \t\n"))

(defun skip-whitespace-backward ()
  (skip-chars-backward " \t\n"))

(defun find-next-left-pair-start ()
  (save-excursion
    (search-backward-regexp "\\((\\|{\\|\\[\\|<\\|'\\|\"\\|`\\)" (point-min) t)
    (let ((start-char (buffer-substring-no-properties (point) (1+ (point)))))
      (cond ((string= "(" start-char) '("(" . ")"))
	    ((string= "[" start-char) '("[" . "]"))
	    ((string= "{" start-char) '("{" . "}"))
	    ((string= "<" start-char) '("<" . ">"))
	    ((string= "'" start-char) '("'" . "'"))
	    ((string= "\"" start-char) '("\"" . "\""))
	    ((string= "`" start-char) '("`" . "`"))))))

(defun dispatch-with-pair (target &optional default)
  (let* ((last-key-seq (this-single-command-keys))
	 (last-key (elt last-key-seq (1- (length last-key-seq))))
	 (next-key (read-char "Pair start character: "))
	 (inner-most-pair (find-next-left-pair-start)))
    (cond ((= next-key ?\() (funcall target "(" ")"))
          ((= next-key ?\{) (funcall target "{" "}"))
          ((= next-key ?\[) (funcall target "[" "]"))
          ((= next-key ?\<) (funcall target "<" ">"))
          ((= next-key ?\') (funcall target "'" "'"))
          ((= next-key ?\") (funcall target "\"" "\""))
	  ((= next-key ?\`) (funcall target "`" "`"))
	  ((and (= next-key last-key) inner-most-pair)
	   (funcall target (car inner-most-pair) (cdr inner-most-pair)))
          (t
	   (when default (funcall default))
	   (pass-events (string next-key))))))

(defun move-point-to-balanced-start (start end)
  (move-point-to-balanced t start end))

(defun move-point-to-balanced-end (start end)
  (move-point-to-balanced nil start end))

(defun move-point-to-balanced (look-for-start start end)
  (let ((counter 1))
    (while (> counter 0)
      (if look-for-start (backward-char 1) (forward-char 1))
      (cond ((looking-at (regexp-quote (if look-for-start end start))) (setq counter (1+ counter)))
            ((looking-at (regexp-quote (if look-for-start start end))) (setq counter (1- counter)))))))

(defun move-point-to-pair-start-simple (pair)
  (backward-char 1)
  (while (not (looking-at (regexp-quote pair)))
    (backward-char 1)))

(defun move-point-to-pair-end-simple (pair)
  (forward-char 1)
  (while (not (looking-at (regexp-quote pair)))
    (forward-char 1)))

(defun move-point-to-pair-starting-string (start end)
  (if (string= start end)
      (move-point-to-pair-start-simple start)
    (move-point-to-balanced-start start end)))

(defun move-point-to-pair-ending-string (start end)
  (if (string= start end)
      (move-point-to-pair-end-simple start)
    (move-point-to-balanced-end start end)))

(defun remove-enclosing-pair ()
  (interactive)
  (dispatch-with-pair 'remove-enclosing-pair-strings))

(defun remove-enclosing-pair-strings (start end)
  (mark-inside-pair-strings start end)
  (let ((start-position (mark)))
    (delete-char (length end))
    (goto-char start-position)
    (delete-char (- (length start)))))

(defun enclose-in-pair ()
  (interactive)
  (unless (region-active-p) (mark-select))
  (dispatch-with-pair 'enclose-in-pair-strings))

(defun enclose-in-pair-strings (start end)
  (let* ((mark-position (mark))
         (point-position (point))
         (start-position (min mark-position point-position))
         (end-position (max mark-position point-position)))
    (goto-char end-position)
    (insert end)
    (goto-char start-position)
    (insert start)
    (goto-char (+ end-position (length end)))))

(defconst text-pairs
  '(("(" . ")")
    ("[" . "]")
    ("{" . "}")
    ("<" . ">")
    ("'" . "'")
    ("\"" . "\"")
    ("`" . "`"))
  "List of cons cells that are the start and end string of a pair")

(defconst left-pair-start-regex
  "\\((\\|{\\|\\[\\|<\\|'\\|\"\\|`\\)"
  "Regex to identify the left start of a pair")

(defconst right-pair-end-regex
  "\\()\\|}\\|\\]\\|>\\|'\\|\"\\|`\\)"
  "Regex to identify the right end of a pair")

(defun move-to-previous-pair-starter ()
  (interactive)
  (let* ((first-pair-starter-pos
	  (save-excursion
	    (search-backward-regexp left-pair-start-regex (point-min) t)
	    (point)))
	 (second-pair-starter-pos
	  (save-excursion
	    (search-backward-regexp left-pair-start-regex (point-min) t 2)
	    (point)))
	 (first-pair-starter-str
	  (save-excursion
	    (goto-char first-pair-starter-pos)
	    (char-at-point))))
    (if (string-equal first-pair-starter-str (cdr (assoc first-pair-starter-str text-pairs)))
	(goto-char second-pair-starter-pos)
      (goto-char first-pair-starter-pos))))

(defun char-at-point ()
  (buffer-substring (point) (1+ (point))))

(defun move-to-next-pair-closer ()
  (interactive)
  (let* ((first-pair-closer-pos
	  (save-excursion
	    (search-forward-regexp right-pair-end-regex (point-max) t)
	    (point)))
	 (first-pair-closer-str
	  (save-excursion
	    (goto-char (1- first-pair-closer-pos))
	    (char-at-point)))
	 (second-pair-closer-pos
	  (save-excursion
	    (search-forward-regexp right-pair-end-regex (point-max) t 2))))
    (if (string-equal first-pair-closer-str (car (rassoc first-pair-closer-str text-pairs)))
	(goto-char second-pair-closer-pos)
      (goto-char first-pair-closer-pos))))

(defun increment-integer-at-point (&optional increment)
  (interactive "p*")
  (update-integer-at-point (lambda (num) (+ num (if increment increment 1)))))

(defun decrement-integer-at-point (&optional decrement)
  (interactive "p*")
  (update-integer-at-point (lambda (num) (- num (if decrement decrement 1)))))

(defun update-integer-at-point (update)
  (let ((offset (skip-chars-backward "0123456789")))
    (if (looking-at "[[:digit:]]+")
	(let* ((number-string (save-excursion
				(re-search-forward "[[:digit:]]+")
				(match-string 0)))
	       (should-pad-p (string-match "0+[[:digit:]]+" number-string))
	       (pad-number #'(lambda (num) (format (concat "%0" (number-to-string (length number-string)) "d") num)))
	       (number (string-to-number number-string))
	       (new-number (funcall update number))
	       (final-string (if should-pad-p (funcall pad-number new-number) (number-to-string new-number))))
	  (delete-region (point) (+ (point) (length number-string)))
	  (insert final-string)
	  (backward-char (+ (length number-string) offset)))
      (message "Can't identify number at point."))))


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

(defun uid ()
  (interactive)
  (let* ((raw (with-temp-buffer
		(shell-command "uuidgen" (current-buffer))
		(buffer-substring (point-min) (point-max))))
	 (uuid (progn (string-match "[ \t\n]*$" raw) (replace-match "" nil nil raw))))
    (insert (substring uuid 0 8))))

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

;; OLD

;;  -------------  ---------------  --------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------------
;; |             ||               ||              ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                        |
;; |     `       ||       1       ||       2      ||       3       ||       4       ||       5       ||       6       ||       7       ||       8       ||       9       ||       0       ||       -       ||       =       ||        backspace       |
;; |             ||               ||              ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                        |
;;  -------------  ---------------  --------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------------

;;   -------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ----------------
;; |                    ||               ||  delete-back  ||replace-w-char || query-rep-rx  || barf-forward  ||   end-buffer  || dumb-jump-back||   recenter    || swiper-tap    ||pop-global-mark|| prev-word-occ || next-word-occ ||                |
;; |       tab          ||       q       ||       d       ||       r       ||       w       ||       b       ||       j       ||       f       ||       u       ||       p       ||       ;       ||       [       ||       ]       ||        \       |
;; |                    ||     q-map     ||     delete    ||   insert-char || qrepl-word-tap|| slurp-forward ||   beg-buffer  ||   dumb-jump   ||   save-buf    ||  swiper-tweak ||   pop-to-mark ||  prev-sym-occ || next-sym-occ  ||  goto-line     |
;;  --------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ----------------

;;  ------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  -----------------------------
;; |                        ||   bol-insert  ||  eol-insert   ||  counsel-yank ||  kill-insert  ||   meta-ctrl   ||prev-pair-start||  back-symbl   ||   page-down   ||    page-up    ||   fwd-symbl   ||next-pair-close||                             |
;; |         control        ||       a       ||       s       ||       h       ||       t       ||       g       ||       y       ||       n       ||       e       ||       o       ||       i       ||       '       ||              return         |
;; |                        ||enclose-in-pair||rm-enclos-pair ||      yank     ||     kill      ||      meta     || begining-line ||     left      ||     down      ||      up       ||     right     ||   end-line    ||                             |
;;  ------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  -----------------------------

;;  ------------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ----------------------------------------
;; |                              ||  win-to-reg   ||   query-repl  ||  macro-toreg  ||   dupl-line   ||  join-line    ||               || avy-in-line   ||               ||               ||               ||                                        |
;; |           shift              ||       z       ||       x       ||       m       ||       c       ||       v       ||       k       ||       l       ||       ,       ||       .       ||       /       ||                shift                   |
;; |                              ||   win-zoom    ||     x-map     ||  jump-to-reg  ||     c-map     ||open-line-below||    apropos    || avy-goto-char || pop-to-mark   ||   ivy-jump    ||    undo       ||                                        |
;;  ------------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ----------------------------------------
