;;
;; tests
;;

(require 'squirrel)

(defconst squirrel--test-data
  '(
	;;
	;; goto-first-child
    ;;
	((name . "goto-first-child")
	 (mode  . go-mode)
	 (commands . (squirrel-goto-first-child squirrel-goto-first-child))
	 (expected-node . "Println")
	 (buffer-contents  . "package main

import \"fmt\"

func main() {
	fmt.Prin#tln(\"hello\")
}
")
	 )
	;;
	;; goto-next
    ;;
	((name . "goto-next")
	 (mode  . go-mode)
	 (commands . (squirrel-goto-next))
	 (expected-node . "Println")
	 (buffer-contents  . "package main

import \"fmt\"

func main() {
	#fmt.Println(\"hello\")
}
")
	 )
	((name . "goto-next end")
	 (mode . go-mode)
	 (commands . (squirrel-goto-next squirrel-goto-next squirrel-goto-next squirrel-goto-next))
	 (expected-node . "Println")
	 (buffer-contents . "package main

import \"fmt\"

func main() {
	#fmt.Println(\"hello\")
}
"))
	;;
	;; goto-parent
    ;;
	((name . "goto-parent")
	 (mode . go-mode)
	 (commands . (squirrel-goto-parent))
	 (expected-node . "fmt.Println")
	 (buffer-contents . "package main

import \"fmt\"

func main() {
	#fmt.Println(\"hello\")
}
"))
	((name . "goto-parent repeat 1")
	 (mode . go-mode)
	 (commands . (squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent))
	 (expected-node . "{
	fmt.Println(\"hello\")
}")
	 (buffer-contents . "package main

import \"fmt\"

func main() {
	#fmt.Println(\"hello\")
}
"))
	((name . "goto-parent repeat 2")
	 (mode . go-mode)
	 (commands . (squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent))
	 (expected-node . "func main() {
	fmt.Println(\"hello\")
}")
	 (buffer-contents . "package main

import \"fmt\"

func main() {
	#fmt.Println(\"hello\")
}
"))
	((name . "goto-parent source file")
	 (mode . go-mode)
	 (commands . (squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent))
	 (expected-node . "package main

import \"fmt\"

func main() {
	fmt.Println(\"hello\")
}
")
	 (buffer-contents . "package main

import \"fmt\"

func main() {
	#fmt.Println(\"hello\")
}
"))
	((name . "goto-parent end")
	 (mode . go-mode)
	 (commands . (squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent squirrel-goto-parent))
	 (expected-node . "package main

import \"fmt\"

func main() {
	fmt.Println(\"hello\")
}
")
	 (buffer-contents . "pa#ckage main

import \"fmt\"

func main() {
	fmt.Println(\"hello\")
}
"))
	;;
	;; mixes: goto-parent & goto-next
	;;
	((name . "goto-parent & goto-next")
	 (mode . go-mode)
	 (commands . (squirrel-goto-parent squirrel-goto-parent squirrel-goto-next))
	 (expected-node . "fmt.Println(\"world\")")
	 (buffer-contents . "package main

import \"fmt\"

func main() {
	f#mt.Println(\"hello\")
	fmt.Println(\"world\")
}
"))
	;;
	;; mixes: goto-parent & goto-previous
	;;
	((name . "goto-parent & goto-previous")
	 (mode . go-mode)
	 (commands . (squirrel-goto-parent squirrel-goto-parent squirrel-goto-previous squirrel-goto-previous))
	 (expected-node . "fmt.Println(\"1\")")
	 (buffer-contents . "package main

import \"fmt\"

func main() {
	fmt.Println(\"1\")
	fmt.Println(\"2\")
	fm#t.Println(\"3\")
}
"))
	;;
	;; mixes: goto-parent & goto-next & goto-first-child
	;;
	((name . "goto-parent & goto-next & goto-first-child")
	 (mode . go-mode)
	 (commands . (squirrel-goto-parent
				  squirrel-goto-parent
				  squirrel-goto-previous
				  squirrel-goto-first-child
				  squirrel-goto-next))
	 (expected-node . "(\"2\")")
	 (buffer-contents . "package main

import \"fmt\"

func main() {
	fmt.Println(\"1\")
	fmt.Println(\"2\")
	fm#t.Println(\"3\")
}
"))
	((name . "goto-parent & goto-next & goto-first-child")
	 (mode . go-mode)
	 (commands . (squirrel-goto-parent
				  squirrel-goto-parent
				  squirrel-goto-previous
				  squirrel-goto-first-child
				  squirrel-goto-first-child
				  squirrel-goto-next))
	 (expected-node . "Println")
	 (buffer-contents . "package main

import \"fmt\"

func main() {
	fmt.Println(\"1\")
	fmt.Println(\"2\")
	fm#t.Println(\"3\")
}
"))
	))

(defun squirrel--test-run (mod buf cmds)
  (with-temp-buffer
	(insert buf)
	(call-interactively mod)
	(call-interactively 'squirrel-mode)
	(add-hook 'squirrel-motion-hook 'squirrel-mark-current-node nil t)
	(goto-char (point-min))
	(search-forward "#")
	(delete-backward-char 1)
	(mapc 'call-interactively cmds)
	(squirrel--node-contents squirrel--current-node)))
	
(defun squirrel--tests-run-all ()
  (dolist (tc squirrel--test-data)
	(let* ((expected (cdr (assoc 'expected-node tc)))
		   (name (cdr (assoc 'name tc)))
		   (mod (cdr (assoc 'mode tc)))
		   (buf (cdr (assoc 'buffer-contents tc)))
		   (cmds (cdr (assoc 'commands tc)))
		   (actual (squirrel--test-run mod buf cmds)))
	  (if (string-equal expected actual)
		  (message "squirrel--tests/%s PASSED" name)
		(message "squirrel--tests/%s FAILED: expected=[%s] actual=[%s]" name expected actual)))))

(squirrel--tests-run-all)

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

