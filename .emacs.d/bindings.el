;;  -------------  ---------------  --------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  --------------------------
;; |             ||               ||              ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                          |
;; |     `       ||       1       ||       2      ||       3       ||       4       ||       5       ||       6       ||       7       ||       8       ||       9       ||       0       ||       -       ||       =       ||        backspace         |
;; |  ucs-insert ||               ||              ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                          |
;;  -------------  ---------------  --------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  --------------------------
;;  --------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------
;; |                    ||  rem-encl-pair|| delete-back   || duplicate-lin ||   join-line   ||     barf      ||               ||               ||               ||     occur     ||               || prev-word-occ || next-word-occ ||                  |
;; |       tab          ||       q       ||       d       ||       r       ||       w       ||       b       ||       j       ||       f       ||       u       ||       p       ||       ;       ||       [       ||       ]       ||        \         |
;; |                    ||  surround     ||     delete    || query-repl-rx ||   open-line   ||     slurp     ||               ||               || avy-goto-char ||    isearch    ||      ag       ||  prev-sym-occ || next-sym-occ  ||  goto-line       |
;;  --------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------
;;  ------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  -------------------------------
;; |                        ||  replace-sel  ||     insert    ||yank-kill-ring ||               ||contract-region||  beg-buffer   ||  back-symbl   ||   page-down   ||    page-up    ||   fwd-symbl   ||  end-buffer   ||                               |
;; |         control        ||       a       ||       s       ||       h       ||       t       ||       g       ||       y       ||       n       ||       e       ||       o       ||       i       ||       '       ||              return           |
;; |                        || replace-char  ||   insert-char ||      yank     ||     kill      || expand-region || begining-line ||     left      ||     down      ||      up       ||     right     ||   end-line    ||                               |
;;  ------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  -------------------------------
;;  ------------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------------------------------
;; |                              ||  ivy-resume   ||               ||               ||               ||               ||               ||               ||               || xref-jump-back||               ||                                          |
;; |           shift              ||       z       ||       x       ||       m       ||       c       ||       v       ||       k       ||       l       ||       ,       ||       .       ||       /       ||                shift                     |
;; |                              ||   repeat      ||     x-map     ||  macro-map    ||     c-map     || save-buffer   ||      jump     ||               ||               || xref-jump-to  ||      undo     ||                                          |
;;  ------------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------------------------------

(defun exit-view-mode ()
  (message "entering view mode, about to disable")
  (view-mode-exit t nil t))

;; xref jump triggers view mode, which is redundant when defaulting to modal mode
(add-hook 'view-mode-hook 'exit-view-mode)

;; clear out some existing C-o bindings
(after 'dired (define-key dired-mode-map (kbd "C-o") nil))
(after 'wdired (define-key wdired-mode-map (kbd "C-o") nil))
(after 'compile (define-key compilation-mode-map (kbd "C-o") nil))
(after 'replace (define-key occur-mode-map (kbd "C-o") nil))
(after 'replace (define-key occur-edit-mode-map (kbd "C-o") nil))
(after 'replace (define-key occur-menu-map (kbd "C-o") nil))
(after 'xref (define-key xref--xref-buffer-mode-map (kbd "C-o") nil))
(after 'vterm (define-key vterm-mode-map (kbd "C-o") nil))
;; so we can rely on C-o to toggle modal mode

(install 'modal)
(global-modal-mode +1)
(define-key global-map (kbd "C-o") 'global-modal-mode)

(install 'expand-region)

(defun switch-to-vterm ()
  (interactive)
  (let* ((buf (get-buffer "vterm")))
    (if buf
	(switch-to-buffer buf)
      (vterm))))

;; c-map
(defconst c-bindings-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)

    (define-key map (kbd "hb") 'help-go-back)
    (define-key map (kbd "hf") 'help-go-back)

    (define-key map (kbd "cc") 'with-editor-finish)
    (define-key map (kbd "cm") 'recompile)
    (define-key map (kbd "cM") 'compile)
    (define-key map (kbd ";") 'comment-dwim)

    (define-key map (kbd "dd") 'dired-hide-details-mode)
    (define-key map (kbd "dw") 'dired-toggle-read-only)

    (define-key map (kbd "ll") 'lsp)
    (define-key map (kbd "lr") 'lsp-rename)
    (define-key map (kbd "la") 'xref-find-apropos)

    (define-key map (kbd "m") 'magit)
    (define-key map (kbd "v") 'switch-to-vterm)

    (define-key map (kbd "en") 'flycheck-next-error)
    (define-key map (kbd "ep") 'flycheck-previous-error)

    (define-key map (kbd "q") 'fill-paragraph)

    map))

;; x-map
(defconst x-bindings-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)

    (define-key map (kbd "a") 'apropos)

    (define-key map (kbd "bb") 'switch-to-buffer)
    (define-key map (kbd "bR") 'rename-buffer)
    (define-key map (kbd "br") 'revert-buffer)
    (define-key map (kbd "bw") 'delete-trailing-whitespace)

    (define-key map (kbd "c") 'save-buffers-kill-terminal)
    (define-key map (kbd "ee") 'eval-last-sexp)
    (define-key map (kbd "eb") 'eval-buffer)
    (define-key map (kbd "ed") 'eval-defun)
    (define-key map (kbd "er") 'eval-region)
    (define-key map (kbd "f") 'find-file)
    (define-key map (kbd "h") 'mark-whole-buffer)
    (define-key map (kbd "k") 'kill-buffer)
    (define-key map (kbd "o") nil)
    (define-key map (kbd "r") 'eval-region)
    (define-key map (kbd "s") 'save-buffer)
    (define-key map (kbd "S") 'save-some-buffers)

    (define-key map (kbd "td") 'toggle-debug-on-error)
    (define-key map (kbd "tf") 'font-lock-mode)
    (define-key map (kbd "tl") 'toggle-truncate-lines)
    (define-key map (kbd "tw") 'leerzeichen-mode)
    (define-key map (kbd "tr") 'rainbow-mode)

    (define-key map (kbd "ve") 'git-gutter:next-hunk)
    (define-key map (kbd "vm") 'git-messenger:popup-message)
    (define-key map (kbd "vo") 'git-gutter:previous-hunk)
    (define-key map (kbd "vp") 'git-gutter:popup-hunk)
    (define-key map (kbd "vr") 'git-gutter:revert-hunk)
    (define-key map (kbd "v<SPC>") 'git-gutter:mark-hunk)

    (define-key map (kbd "x") 'm-x-with-bindings)

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

(define-key modal-mode-map (kbd "M-`") 'ucs-insert)
(define-key modal-mode-map (kbd "`") 'ucs-insert)
(define-key modal-mode-map (kbd "<SPC>") 'mark-select)
(define-key modal-mode-map (kbd "+") 'increment-integer-at-point)
(define-key modal-mode-map (kbd "-") 'decrement-integer-at-point)

(define-key modal-mode-map (kbd "q") 'enclose-in-pair)
(define-key modal-mode-map (kbd "Q") 'remove-enclosing-pair)
(define-key modal-mode-map (kbd "d") 'delete-forward-char)
(define-key modal-mode-map (kbd "D") 'delete-backward-char)
(define-key modal-mode-map (kbd "r") 'vr/query-replace)
(define-key modal-mode-map (kbd "R") 'duplicate-line)
(define-key modal-mode-map (kbd "w") 'fg/open-line-above)
(define-key modal-mode-map (kbd "W") 'fg/open-line-below)
(define-key modal-mode-map (kbd "b") 'slurp-forward)
(define-key modal-mode-map (kbd "B") 'barf-forward)
(define-key modal-mode-map (kbd "j") 'join-line)
(define-key modal-mode-map (kbd "J") nil)

(require '3w)

;; TODO f map?
(define-key modal-mode-map (kbd "f1") '3w-split-1)
(define-key modal-mode-map (kbd "f2") '3w-split-2)
(define-key modal-mode-map (kbd "f3") '3w-split-3)
(define-key modal-mode-map (kbd "f4") '3w-split-2-1)
(define-key modal-mode-map (kbd "fb") 'balance-windows)
(define-key modal-mode-map (kbd "fa") '3w-jump-1)
(define-key modal-mode-map (kbd "fs") '3w-jump-2)
(define-key modal-mode-map (kbd "fh") '3w-jump-3)
(define-key modal-mode-map (kbd "ft") '3w-toggle-side-window)
(define-key modal-mode-map (kbd "fo") 'other-window)

(define-key modal-mode-map (kbd "F") nil)
(define-key modal-mode-map (kbd "u") 'avy-goto-char-timer)
(define-key modal-mode-map (kbd "U") nil)
(define-key modal-mode-map (kbd "p") 'isearch-forward)
(define-key modal-mode-map (kbd "P") 'occur)
(define-key modal-mode-map (kbd ";") 'ag)
(define-key modal-mode-map (kbd ":") 'nil)
(define-key modal-mode-map (kbd "[") 'move-to-previous-symbol-occurrence)
(define-key modal-mode-map (kbd "{") 'move-to-previous-word-occurrence)
(define-key modal-mode-map (kbd "]") 'move-to-next-symbol-occurrence)
(define-key modal-mode-map (kbd "}") 'move-to-next-word-occurrence)
(define-key modal-mode-map (kbd "\\") 'goto-line)
(define-key modal-mode-map (kbd "|") nil)

(define-key modal-mode-map (kbd "a") 'fg/replace-char)
(define-key modal-mode-map (kbd "A") 'replace-select)
(define-key modal-mode-map (kbd "s") 'fg/insert-char)
(define-key modal-mode-map (kbd "S") 'insert-literal)
(define-key modal-mode-map (kbd "h") 'yank)
(define-key modal-mode-map (kbd "H") 'yank-from-kill-ring)
(define-key modal-mode-map (kbd "t") 'kill-select)
(define-key modal-mode-map (kbd "T") nil)
(define-key modal-mode-map (kbd "g") 'er/expand-region)
(define-key modal-mode-map (kbd "G") 'er/contract-region)
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

(define-key modal-mode-map (kbd "z") 'repeat)
(define-key modal-mode-map (kbd "Z") 'ivy-resume)
(define-key modal-mode-map (kbd "x") x-bindings-map)
(define-key modal-mode-map (kbd "X") nil)
(define-key modal-mode-map (kbd "m") macro-bindings-map)
(define-key modal-mode-map (kbd "M") nil)
(define-key modal-mode-map (kbd "c") c-bindings-map)
(define-key modal-mode-map (kbd "C") nil)
(define-key modal-mode-map (kbd "v") 'save-buffer)
(define-key modal-mode-map (kbd "V") nil)
(define-key modal-mode-map (kbd "k") 'jump)
(define-key modal-mode-map (kbd "K") nil)
(define-key modal-mode-map (kbd "l") nil)
(define-key modal-mode-map (kbd "L") nil)
(define-key modal-mode-map (kbd ",") nil)
(define-key modal-mode-map (kbd "<") 'nil)
(define-key modal-mode-map (kbd ".") 'xref-find-definitions)
(define-key modal-mode-map (kbd ">") 'xref-pop-marker-stack)
(define-key modal-mode-map (kbd "/") 'undo)
(define-key modal-mode-map (kbd "?") nil)

(define-key modal-mode-map (kbd "S-<left>") 'shrink-window-horizontally)
(define-key modal-mode-map (kbd "S-<right>") 'enlarge-window-horizontally)
(define-key modal-mode-map (kbd "S-<up>") 'enlarge-window)
(define-key modal-mode-map (kbd "S-<down>") 'shrink-window)

;;
;; BLANK
;;
;  -------------  ---------------  --------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  --------------------------
;; |             ||               ||              ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                          |
;; |     `       ||       1       ||       2      ||       3       ||       4       ||       5       ||       6       ||       7       ||       8       ||       9       ||       0       ||       -       ||       =       ||        backspace         |
;; |             ||               ||              ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                          |
;;  -------------  ---------------  --------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  --------------------------
;;  --------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------
;; |                    ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                  |
;; |       tab          ||       q       ||       d       ||       r       ||       w       ||       b       ||       j       ||       f       ||       u       ||       p       ||       ;       ||       [       ||       ]       ||        \         |
;; |                    ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                  |
;;  --------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------
;;  ------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  -------------------------------
;; |                        ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                               |
;; |         control        ||       a       ||       s       ||       h       ||       t       ||       g       ||       y       ||       n       ||       e       ||       o       ||       i       ||       '       ||              return           |
;; |                        ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                               |
;;  ------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  -------------------------------
;;  ------------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------------------------------
;; |                              ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                                          |
;; |           shift              ||       z       ||       x       ||       m       ||       c       ||       v       ||       k       ||       l       ||       ,       ||       .       ||       /       ||                shift                     |
;; |                              ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                                          |
;;  ------------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------------------------------
