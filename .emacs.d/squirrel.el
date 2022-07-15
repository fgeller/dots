(require 'squirrel)

(defun fg/enable-squirrel ()
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (when (and (not squirrel-mode)
			 (assoc major-mode tree-sitter-major-mode-language-alist))
	(define-key modal-mode-map (kbd "I") 'squirrel-goto-next)
	(define-key modal-mode-map (kbd "N") 'squirrel-goto-previous)
	(define-key modal-mode-map (kbd "E") 'squirrel-goto-first-child)
	(define-key modal-mode-map (kbd "O") 'squirrel-goto-parent)
	(add-hook 'squirrel-motion-hook 'squirrel-mark-current-node)
	(squirrel-mode +1)))

;; (after 'go-mode (add-hook 'go-mode-hook 'fg/enable-squirrel))
;; (after 'js (add-hook 'js-mode-hook 'fg/enable-squirrel))
;; (after 'mhtml-mode (add-hook 'mhtml-mode-hook 'fg/enable-squirrel))
;; (after 'yaml-mode (add-hook 'yaml-mode-hook 'fg/enable-squirrel))

(add-hook 'after-change-major-mode-hook 'fg/enable-squirrel)

;; after-change-major-mode-hook
;; tree-sitter-major-mode-language-alist

;; (defun fg/test-hello ()
;;   (message "HELSHIOTENSIHOETNISOHETNISHOTEN"))
;; (add-hook 'yaml-mode-hook 'fg/test-hello)


;; ;; -*- lexical-binding: t -*-

;; ;; goals:
;; ;;  - navigation in tree
;; ;;  - navigation to beginning and end of node
;; ;;  - hook to motion command (for marking node)

;; ;; depends on:
;; ;;
;; ;; tree-sitter

;; (require 'tree-sitter)

;; (defun squirrel--goto-node (next-node-finder)
;;   (let* ((current-node (squirrel--ensure-current-node))
;; 		 (next-node (funcall next-node-finder current-node)))
;; 	(when next-node
;; 	  (squirrel--log "motion: current-node=%s " (squirrel--node-to-string current-node))
;; 	  (squirrel--log "motion: next-node=%s " (squirrel--node-to-string next-node))
;; 	  (setq squirrel--current-node next-node)
;; 	  (goto-char (tsc-node-start-position next-node))
;; 	  (run-hooks 'squirrel-motion-hook))))

;; (defun squirrel-goto-next ()
;;   (interactive)
;;   (squirrel--goto-node
;;    (lambda (c) (tsc-get-next-named-sibling c))))

;; (defun squirrel-goto-previous ()
;;   (interactive)
;;   (squirrel--goto-node
;;    (lambda (c) (tsc-get-prev-named-sibling c))))

;; (defun squirrel-goto-parent ()
;;   (interactive)
;;   (squirrel--goto-node
;;    (lambda (c) (tsc-get-parent c))))

;; (defun squirrel-goto-first-child ()
;;   (interactive)
;;   (squirrel--goto-node
;;    (lambda (c) (tsc-get-nth-named-child c 0))))

;; ;; TODO squirrel-goto-node-start
;; ;; TODO squirrel-goto-node-end

;; (defun squirrel-mark-current-node ()
;;   (let* ((c (squirrel--ensure-current-node)))
;; 	(set-mark (tsc-node-end-position c)))) ;; TODO push-mark?

;; (defun squirrel--point-within-node-p ()
;;   (let ((p (point)))
;; 	(or (= p (tsc-node-start-position squirrel--current-node))
;; 		(= p (tsc-node-end-position squirrel--current-node)))))

;; (defun squirrel--ensure-current-node ()
;;   ;; if not current node
;;   ;; if point not in current node
;;   (when (or (not squirrel--current-node)
;; 			(not (squirrel--point-within-node-p)))
;; 	(setq squirrel--current-node (tree-sitter-node-at-pos :named)))
;;   (squirrel--log "ensure-current-node: n=[%s]" (squirrel--node-to-string squirrel--current-node))
;;   squirrel--current-node)

;; (defvar-local squirrel--current-node nil)

;; (defconst squirrel-motion-hook nil)

;; (defconst squirrel--should-log nil)

;; (defun squirrel--log (msg &rest args)
;;   (when squirrel--should-log
;; 	(apply 'message (cons (concat ">> squirrel: " msg) args))))

;; (defun squirrel--node-to-string (n)
;;   (format "node[type=%s src=%s]"
;; 		  (tsc-node-type n)
;; 		  (squirrel--node-contents n)))

;; (defun squirrel--node-contents (n)
;;   (buffer-substring-no-properties (tsc-node-start-position n)
;; 								  (tsc-node-end-position n)))

;; (defun squirrel--teardown ()
;;   (setq squirrel--current-node nil))

;; (defun squirrel--setup ()
;;   (setq squirrel--current-node nil))

;; (defconst squirrel-mode-map (make-sparse-keymap))

;; ;;;###autoload
;; (define-minor-mode squirrel-mode
;;   "Minor mode that provides navigation and mark functions for tree-sitter supported languages.
;; "
;;   :lighter " sq"
;;   :keymap squirrel-mode-map
;;   :group 'squirrel
;;   (tree-sitter--handle-dependent squirrel-mode 'squirrel--setup 'squirrel--teardown))

;; (provide 'squirrel)

;; ;;
;; ;; tests
;; ;;

;; (defconst squirrel--test-data
;;   '(
;; 	;;
;; 	;; goto-first-child
;;     ;;
;; 	((name . "goto-first-child")
;; 	 (mode  . go-mode)
;; 	 (commands . (squirrel-goto-first-child squirrel-goto-first-child))
;; 	 (expected-node . "Println")
;; 	 (buffer-contents  . "package main

;; import \"fmt\"

;; func main() {
;; 	fmt.Prin#tln(\"hello\")
;; }
;; ")
;; 	 )
;; 	;;
;; 	;; goto-next
;;     ;;
;; 	((name . "goto-next")
;; 	 (mode  . go-mode)
;; 	 (commands . (squirrel-goto-next))
;; 	 (expected-node . "Println")
;; 	 (buffer-contents  . "package main

;; import \"fmt\"

;; func main() {
;; 	#fmt.Println(\"hello\")
;; }
;; ")
;; 	 )
;; 	((name . "goto-next end")
;; 	 (mode . go-mode)
;; 	 (commands . (squirrel-goto-next squirrel-goto-next squirrel-goto-next squirrel-goto-next))
;; 	 (expected-node . "Println")
;; 	 (buffer-contents . "package main

;; import \"fmt\"

;; func main() {
;; 	#fmt.Println(\"hello\")
;; }
;; "))
;; 	;;
;; 	;; goto-parent
;;     ;;
;; 	((name . "goto-parent")
;; 	 (mode . go-mode)
;; 	 (commands . (squirrel-goto-parent))
;; 	 (expected-node . "fmt.Println")
;; 	 (buffer-contents . "package main

;; import \"fmt\"

;; func main() {
;; 	#fmt.Println(\"hello\")
;; }
;; "))
;; 	((name . "goto-parent repeat 1")
;; 	 (mode . go-mode)
;; 	 (commands . (squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent))
;; 	 (expected-node . "{
;; 	fmt.Println(\"hello\")
;; }")
;; 	 (buffer-contents . "package main

;; import \"fmt\"

;; func main() {
;; 	#fmt.Println(\"hello\")
;; }
;; "))
;; 	((name . "goto-parent repeat 2")
;; 	 (mode . go-mode)
;; 	 (commands . (squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent))
;; 	 (expected-node . "func main() {
;; 	fmt.Println(\"hello\")
;; }")
;; 	 (buffer-contents . "package main

;; import \"fmt\"

;; func main() {
;; 	#fmt.Println(\"hello\")
;; }
;; "))
;; 	((name . "goto-parent source file")
;; 	 (mode . go-mode)
;; 	 (commands . (squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent))
;; 	 (expected-node . "package main

;; import \"fmt\"

;; func main() {
;; 	fmt.Println(\"hello\")
;; }
;; ")
;; 	 (buffer-contents . "package main

;; import \"fmt\"

;; func main() {
;; 	#fmt.Println(\"hello\")
;; }
;; "))
;; 	((name . "goto-parent end")
;; 	 (mode . go-mode)
;; 	 (commands . (squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent))
;; 	 (expected-node . "package main

;; import \"fmt\"

;; func main() {
;; 	fmt.Println(\"hello\")
;; }
;; ")
;; 	 (buffer-contents . "pa#ckage main

;; import \"fmt\"

;; func main() {
;; 	fmt.Println(\"hello\")
;; }
;; "))
;; 	;;
;; 	;; mixes: goto-parent & goto-next
;; 	;;
;; 	((name . "goto-parent & goto-next")
;; 	 (mode . go-mode)
;; 	 (commands . (squirrel-goto-parent squirrel-goto-parent squirrel-goto-next))
;; 	 (expected-node . "fmt.Println(\"world\")")
;; 	 (buffer-contents . "package main

;; import \"fmt\"

;; func main() {
;; 	f#mt.Println(\"hello\")
;; 	fmt.Println(\"world\")
;; }
;; "))
;; 	;;
;; 	;; mixes: goto-parent & goto-previous
;; 	;;
;; 	((name . "goto-parent & goto-previous")
;; 	 (mode . go-mode)
;; 	 (commands . (squirrel-goto-parent squirrel-goto-parent squirrel-goto-previous squirrel-goto-previous))
;; 	 (expected-node . "fmt.Println(\"1\")")
;; 	 (buffer-contents . "package main

;; import \"fmt\"

;; func main() {
;; 	fmt.Println(\"1\")
;; 	fmt.Println(\"2\")
;; 	fm#t.Println(\"3\")
;; }
;; "))
;; 	;;
;; 	;; mixes: goto-parent & goto-next & goto-first-child
;; 	;;
;; 	((name . "goto-parent & goto-next & goto-first-child")
;; 	 (mode . go-mode)
;; 	 (commands . (squirrel-goto-parent
;; 				  squirrel-goto-parent
;; 				  squirrel-goto-previous
;; 				  squirrel-goto-first-child
;; 				  squirrel-goto-next))
;; 	 (expected-node . "(\"2\")")
;; 	 (buffer-contents . "package main

;; import \"fmt\"

;; func main() {
;; 	fmt.Println(\"1\")
;; 	fmt.Println(\"2\")
;; 	fm#t.Println(\"3\")
;; }
;; "))
;; 	((name . "goto-parent & goto-next & goto-first-child")
;; 	 (mode . go-mode)
;; 	 (commands . (squirrel-goto-parent
;; 				  squirrel-goto-parent
;; 				  squirrel-goto-previous
;; 				  squirrel-goto-first-child
;; 				  squirrel-goto-first-child
;; 				  squirrel-goto-next))
;; 	 (expected-node . "Println")
;; 	 (buffer-contents . "package main

;; import \"fmt\"

;; func main() {
;; 	fmt.Println(\"1\")
;; 	fmt.Println(\"2\")
;; 	fm#t.Println(\"3\")
;; }
;; "))
;; 	))

;; (defun squirrel--test-run (mod buf cmds)
;;   (with-temp-buffer
;; 	(insert buf)
;; 	(call-interactively mod)
;; 	(call-interactively 'squirrel-mode)
;; 	(add-hook 'squirrel-motion-hook 'squirrel-mark-current-node nil t)
;; 	(goto-char (point-min))
;; 	(search-forward "#")
;; 	(delete-backward-char 1)
;; 	(mapc 'call-interactively cmds)
;; 	(squirrel--node-contents squirrel--current-node)))
	
;; (defun squirrel--tests-run-all ()
;;   (dolist (tc squirrel--test-data)
;; 	(let* ((expected (cdr (assoc 'expected-node tc)))
;; 		   (name (cdr (assoc 'name tc)))
;; 		   (mod (cdr (assoc 'mode tc)))
;; 		   (buf (cdr (assoc 'buffer-contents tc)))
;; 		   (cmds (cdr (assoc 'commands tc)))
;; 		   (actual (squirrel--test-run mod buf cmds)))
;; 	  (if (string-equal expected actual)
;; 		  (message "squirrel--tests/%s PASSED" name)
;; 		(message "squirrel--tests/%s FAILED: expected=[%s] actual=[%s]" name expected actual)))))

;; (squirrel--tests-run-all)

;; source_file:
;;   package_clause:
;;     package_identifier:
;;   import_declaration:
;;     import_spec:
;;       interpreted_string_literal:
;;   function_declaration:
;;     identifier:
;;     parameter_list:
;;     block:
;;       call_expression:
;;         selector_expression:
;;           identifier:
;;           field_identifier:
;;         argument_list:
;;           interpreted_string_literal:
;;       call_expression:
;;         selector_expression:
;;           identifier:
;;           field_identifier:
;;         argument_list:
;;           interpreted_string_literal:

