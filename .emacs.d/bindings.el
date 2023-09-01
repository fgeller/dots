;;  -------------  ---------------  --------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  --------------------------
;; |             ||               ||              ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                          |
;; |     `       ||       1       ||       2      ||       3       ||       4       ||       5       ||       6       ||       7       ||       8       ||       9       ||       0       ||       -       ||       =       ||        backspace         |
;; | emoji-search||               ||              ||               ||               ||               ||               ||               ||               ||               ||               ||               ||               ||                          |
;;  -------------  ---------------  --------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  --------------------------
;;  --------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------
;; |                    ||  rem-encl-pair|| zap-up-to-char|| duplicate-lin ||  bol-insert   || ola-insert    ||               ||backward-to-chr||               ||     occur     ||               || prev-word-occ || next-word-occ ||                  |
;; |       tab          ||       q       ||       d       ||       r       ||       w       ||       b       ||       j       ||       f       ||       u       ||       p       ||       ;       ||       [       ||       ]       ||        \         |
;; |                    ||  surround     ||     delete    || query-repl-rx ||  eol-insert   || olb-insert    ||   join-line   ||forward-to-char|| avy-goto-char ||    isearch    ||consult-git-grp||  prev-sym-occ || next-sym-occ  ||consult-goto-line |
;;  --------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------
;;  ------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  -------------------------------
;; |                        || replace-char  ||     insert    ||yank-kill-ring ||               ||               ||  beg-buffer   ||  back-symbl   ||   page-down   ||    page-up    ||   fwd-symbl   ||  end-buffer   ||                               |
;; |         control        ||       a       ||       s       ||       h       ||       t       ||       g       ||       y       ||       n       ||       e       ||       o       ||       i       ||       '       ||              return           |
;; |                        ||  replace-sel  ||   insert-char ||      yank     ||     kill      ||  consult-buf  || begining-line ||     left      ||     down      ||      up       ||     right     ||   end-line    ||                               |
;;  ------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  -------------------------------
;;  ------------------------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ---------------  ------------------------------------------
;; |                              ||  ivy-resume   ||               ||               ||               || backward-sexp ||bol-modal-deact||eol-modal-deact||               || xref-jump-back||               ||                                          |
;; |           shift              ||       z       ||       x       ||       m       ||       c       ||       v       ||       k       ||       l       ||       ,       ||       .       ||       /       ||                shift                     |
;; |                              ||   repeat      ||     x-map     ||  macro-map    ||     c-map     || forward-sexp  ||aft-indent-deac||modal-deactivat||xref-jump-back || xref-jump-to  ||      undo     ||                                          |
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
(define-key Buffer-menu-mode-map (kbd "C-o") nil)
;; so we can rely on C-o to toggle modal mode


;; hacks to get esc to work as a keypress (not M- or ESC ESC ESC) in terminal
;; https://evil.readthedocs.io/en/latest/faq.html?highlight=terminal#problems-with-the-escape-key-in-the-terminal
(install 'evil)
(require 'evil-core)
(evil-esc-mode +1)

(defun fg/esc (map)
  "Drops some guards from evil's `evil-esc' so the ESC event is always translated"
  (if (and (let ((keys (this-single-command-keys)))
             (and (> (length keys) 0)
                  (= (aref keys (1- (length keys))) ?\e)))
           (sit-for evil-esc-delay))
      (prog1 [escape]
        (when defining-kbd-macro
          (end-kbd-macro)
          (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
          (start-kbd-macro t t)))
    map))
(advice-add 'evil-esc :override 'fg/esc)

(install 'modal)
(global-modal-mode +1)

(define-key global-map (kbd "C-o") 'modal-mode-deactivate)
;; this would prevent access to M- in tty
;; (define-key global-map (kbd "ESC") 'modal-mode-activate)
(define-key global-map (kbd "<escape>") 'modal-mode-activate)

(define-key global-map (kbd "C-j") 'yas-expand)
(after 'org (define-key org-mode-map (kbd "C-j") 'yas-expand))

(install 'expand-region)

(require '3w)

(define-key global-map (kbd "M-.") 'embark-act)
(define-key minibuffer-local-map (kbd "M-.") 'embark-act)

(load-custom "~/.emacs.d/motion.el")
(load-custom "~/.emacs.d/edit.el")

;; c-map
(defconst c-bindings-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)

    (define-key map (kbd "hb") 'help-go-back)
    (define-key map (kbd "hf") 'help-go-back)

    (define-key map (kbd "b") 'browse-url)

    (define-key map (kbd "cc") 'with-editor-finish)
    (define-key map (kbd "cm") 'recompile)
    (define-key map (kbd "cM") 'fg/project-compile)
    (define-key map (kbd ";") 'comment-dwim)

    (define-key map (kbd "dt") 'fg/go-run-this-test)
    (define-key map (kbd "dT") 'fg/go-run-all-tests)
    (define-key map (kbd "dr") 'fg/go-recompile)
    (define-key map (kbd "di") 'fg/imenu)
    (define-key map (kbd "dn") 'fg/go-goto-next-error)
    (define-key map (kbd "dp") 'fg/go-goto-previous-error)

    (define-key map (kbd "lb") 'dap-breakpoint-add)
    (define-key map (kbd "lc") 'consult-compile-error)
    (define-key map (kbd "le") 'consult-lsp-diagnostics)
    (define-key map (kbd "ll") 'lsp)
    (define-key map (kbd "lR") 'lsp-rename)
    (define-key map (kbd "lr") 'lsp-find-references)
    (define-key map (kbd "la") 'consult-lsp-symbols)
    (define-key map (kbd "lA") 'consult-lsp-file-symbols)
    (define-key map (kbd "ld") 'dap-debug)
    (define-key map (kbd "li") 'lsp-find-implementation)

    (define-key map (kbd "m") 'outline-cycle)
    (define-key map (kbd "M") 'outline-cycle-buffer)

    (define-key map (kbd "oL") 'org-store-link)
    (define-key map (kbd "ol") 'org-insert-link)
    (define-key map (kbd "oc") 'org-capture)
    (define-key map (kbd "ot") 'org-todo)
    (define-key map (kbd "os") 'org-insert-structure-template)

    (define-key map (kbd "en") 'flycheck-next-error)
    (define-key map (kbd "ep") 'flycheck-previous-error)

    (define-key map (kbd "p") 'fg/visit-project)

    (define-key map (kbd "tp") 'tree-edit-goto-desired-parent)

    (define-key map (kbd "v") 'vc-prefix-map)
	(define-key vc-prefix-map (kbd "a") 'vc-annotate)
	(define-key vc-prefix-map (kbd "c") 'fg/pick-rev)
	(define-key vc-prefix-map (kbd "d") 'fg/vc-dir-project)
	(define-key vc-prefix-map (kbd "e") 'fg/vc-ediff-wrapper)
	(define-key vc-prefix-map (kbd "f") 'fg/git-fetch)
	(define-key vc-prefix-map (kbd "F") 'vc-update)
	(define-key vc-prefix-map (kbd "g") nil)
	(define-key vc-prefix-map (kbd "gp") 'fg/github-open-pull-request)
	(define-key vc-prefix-map (kbd "k") 'vc-revert)
	(define-key vc-prefix-map (kbd "K") 'vc-delete-file)
	(define-key vc-prefix-map (kbd "m") 'fg/git-ff-main)
	(define-key vc-prefix-map (kbd "M") 'fg/git-merge-main)
	(define-key vc-prefix-map (kbd "n") 'fg/new-branch)
	(define-key vc-prefix-map (kbd "o") 'fg/branch-overview)
	(define-key vc-prefix-map (kbd "p") 'fg/github-open-pull-request)
	(define-key vc-prefix-map (kbd "P") 'vc-push)
	(define-key vc-prefix-map (kbd "s") 'fg/vc-git-show)

    (define-key map (kbd "q") 'fill-paragraph)

    ;; TODO cw -> toggle wdired and wrep
    map))

;; x-map
(defconst x-bindings-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)

    (define-key map (kbd "a") 'apropos)

    (define-key map (kbd "bb") 'switch-to-buffer)
    (define-key map (kbd "br") 'rename-buffer)
    (define-key map (kbd "bg") 'revert-buffer-quick)
    (define-key map (kbd "bw") 'delete-trailing-whitespace)

    (define-key map (kbd "c") 'save-buffers-kill-terminal)
    (define-key map (kbd "ee") 'eval-last-sexp)
    (define-key map (kbd "eb") 'eval-buffer)
    (define-key map (kbd "ed") 'eval-defun)
    (define-key map (kbd "er") 'eval-region)
    (define-key map (kbd "f") 'find-file)
    (define-key map (kbd "g") 'revert-buffer-quick)
    (define-key map (kbd "h") 'mark-whole-buffer)
    (define-key map (kbd "k") 'kill-buffer)

	(define-key map (kbd "le") 'eshell)

    (define-key map (kbd "o") nil)
    (define-key map (kbd "r") 'eval-region)
    (define-key map (kbd "s") 'save-buffer)
    (define-key map (kbd "S") 'save-some-buffers)

    (define-key map (kbd "td") 'toggle-debug-on-error)
    (define-key map (kbd "tf") 'font-lock-mode)
    (define-key map (kbd "tl") 'toggle-truncate-lines)
    (define-key map (kbd "tw") 'leerzeichen-mode)
    (define-key map (kbd "tr") 'rainbow-mode)
    (define-key map (kbd "tn") 'nlinum-mode)
    (define-key map (kbd "tu") 'markdown-toggle-url-hiding)
    (define-key map (kbd "th") 'global-highlight-thing-mode)

    (define-key map (kbd "vg") 'vc-annotate)
    (define-key map (kbd "vn") 'diff-hl-next-hunk)
    (define-key map (kbd "vp") 'diff-hl-previous-hunk)
    (define-key map (kbd "vr") 'diff-hl-revert-hunk)

    (define-key map (kbd "x") 'execute-extended-command)

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


(defconst @-bindings-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)

    (define-key map (kbd "a") 'outline-show-all)
    (define-key map (kbd "b") 'outline-backward-same-level)
    (define-key map (kbd "c") 'outline-hide-entry)
    (define-key map (kbd "d") 'outline-hide-subtree)
    (define-key map (kbd "e") 'outline-show-entry)
    (define-key map (kbd "f") 'outline-forward-same-level)
    (define-key map (kbd "TAB") 'outline-show-children)
    (define-key map (kbd "k") 'outline-show-branches)
    (define-key map (kbd "l") 'outline-hide-leaves)
    (define-key map (kbd "RET") 'outline-insert-heading)
    (define-key map (kbd "RET") 'insert-heading)
	(define-key map (kbd "n") 'outline-next-visible-heading)
	(define-key map (kbd "o") 'outline-hide-other)
	(define-key map (kbd "p") 'outline-previous-visible-heading)
	(define-key map (kbd "q") 'outline-hide-sublevels)
	(define-key map (kbd "s") 'outline-show-subtree)
	(define-key map (kbd "t") 'outline-hide-body)
	(define-key map (kbd "u") 'outline-up-heading)
	(define-key map (kbd "v") 'outline-move-subtree-down)
	(define-key map (kbd "^") 'outline-move-subtree-up)
	(define-key map (kbd "@") 'outline-mark-subtree)
	(define-key map (kbd "<") 'outline-promote)
	(define-key map (kbd ">") 'outline-demote)

	map))

(define-key modal-mode-map (kbd "M-`") 'emoji-search)
(define-key modal-mode-map (kbd "`") 'emoji-search)
(define-key modal-mode-map (kbd "<SPC>") 'fg/mark-select)
(define-key modal-mode-map (kbd "+") 'increment-integer-at-point)
(define-key modal-mode-map (kbd "-") 'decrement-integer-at-point)
(define-key modal-mode-map (kbd "%") 'fg/jump-to-matching-paren)

(define-key modal-mode-map (kbd "@") @-bindings-map)

(define-key modal-mode-map (kbd "q") 'enclose-in-pair)
(define-key modal-mode-map (kbd "Q") 'remove-enclosing-pair)
(define-key modal-mode-map (kbd "d") 'delete-forward-char)
(define-key modal-mode-map (kbd "D") 'zap-up-to-char)
(define-key modal-mode-map (kbd "r") 'vr/query-replace)
(define-key modal-mode-map (kbd "R") 'duplicate-line)
;; (define-key modal-mode-map (kbd "W") 'fg/backward-to-char)
;; (define-key modal-mode-map (kbd "w") 'fg/forward-to-char)
(define-key modal-mode-map (kbd "W") 'fg/beginning-of-line-insert)
(define-key modal-mode-map (kbd "w") 'fg/end-of-line-insert)
(define-key modal-mode-map (kbd "B") 'fg/open-line-above-insert)
(define-key modal-mode-map (kbd "b") 'fg/open-line-below-insert)
(define-key modal-mode-map (kbd "j") 'join-line)
(define-key modal-mode-map (kbd "J") nil)

(define-key modal-mode-map (kbd "f") 3w-map)
(define-key 3w-map (kbd "z") 'fg/store-window-layout)
(define-key 3w-map (kbd "Z") 'fg/restore-window-layout)
(define-key modal-mode-map (kbd "F") nil)
(define-key modal-mode-map (kbd "u") 'fg/jump-to-char)
(define-key modal-mode-map (kbd "U") nil)
(define-key modal-mode-map (kbd "p") 'fg/consult-line)
(define-key modal-mode-map (kbd "P") 'occur)
(define-key modal-mode-map (kbd ";") 'fg/grep)
(define-key modal-mode-map (kbd ":") 'fg/rg)
(define-key modal-mode-map (kbd "[") 'fg/move-to-previous-symbol-occurrence)
(define-key modal-mode-map (kbd "{") 'fg/move-to-previous-word-occurrence)
(define-key modal-mode-map (kbd "]") 'fg/move-to-next-symbol-occurrence)
(define-key modal-mode-map (kbd "}") 'fg/move-to-next-word-occurrence)
(define-key modal-mode-map (kbd "\\") 'fg/consult-goto-line)
(define-key modal-mode-map (kbd "1") 'fg/consult-goto-line)
(define-key modal-mode-map (kbd "|") 'mc/edit-lines)

(define-key modal-mode-map (kbd "a") 'fg/replace-select)
(define-key modal-mode-map (kbd "A") 'fg/replace-char)
(define-key modal-mode-map (kbd "s") 'fg/insert-char)
(define-key modal-mode-map (kbd "S") 'fg/insert-literal)
(define-key modal-mode-map (kbd "h") 'yank)
(define-key modal-mode-map (kbd "H") 'consult-yank-from-kill-ring)
(define-key modal-mode-map (kbd "t") 'kill-select)
(define-key modal-mode-map (kbd "T") nil)
(define-key modal-mode-map (kbd "g") 'fg/jump)
(define-key modal-mode-map (kbd "G") nil)
(define-key modal-mode-map (kbd "y") 'move-beginning-of-line)
(define-key modal-mode-map (kbd "Y") 'fg/beginning-of-buffer)
(define-key modal-mode-map (kbd "n") 'left-char)
(define-key modal-mode-map (kbd "N") 'fg/back-node-or-word)
(define-key modal-mode-map (kbd "e") 'next-line)
(define-key modal-mode-map (kbd "E") 'fg/down-node-or-scroll)
(define-key modal-mode-map (kbd "o") 'previous-line)
(define-key modal-mode-map (kbd "O") 'fg/up-node-or-scroll)
(define-key modal-mode-map (kbd "i") 'right-char)
(define-key modal-mode-map (kbd "I") 'fg/forward-node-or-word)
(define-key modal-mode-map (kbd "'") 'move-end-of-line)
(define-key modal-mode-map (kbd "\"") 'fg/end-of-buffer)

(define-key modal-mode-map (kbd "z") 'fg/repeat-last-edit)
(define-key modal-mode-map (kbd "Z") 'ivy-resume)
(define-key modal-mode-map (kbd "x") x-bindings-map)
(define-key modal-mode-map (kbd "X") nil)
(define-key modal-mode-map (kbd "m") macro-bindings-map)
(define-key modal-mode-map (kbd "M") nil)
(define-key modal-mode-map (kbd "c") c-bindings-map)
(define-key modal-mode-map (kbd "C") nil)
(define-key modal-mode-map (kbd "v") 'forward-sexp)
(define-key modal-mode-map (kbd "V") 'backward-sexp)
(define-key modal-mode-map (kbd "k") 'after-indent-modal-mode-deactivate)
(define-key modal-mode-map (kbd "K") 'bol-modal-mode-deactivate)
(define-key modal-mode-map (kbd "l") 'modal-mode-deactivate)
(define-key modal-mode-map (kbd "L") 'eol-modal-mode-deactivate)
(define-key modal-mode-map (kbd ",") 'xref-pop-marker-stack)
(define-key modal-mode-map (kbd "<") nil)
(define-key modal-mode-map (kbd ".") 'xref-find-definitions)
(define-key modal-mode-map (kbd ">") 'xref-pop-marker-stack)
(define-key modal-mode-map (kbd "/") 'undo)
(define-key modal-mode-map (kbd "?") 'xref-find-references)

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
