(add-to-list 'treesit-language-source-alist
             '(tsx "https://github.com/tree-sitter/tree-sitter-typescript"
                   "master" "tsx/src"))
;; optional but handy
(add-to-list 'treesit-language-source-alist
             '(typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                          "master" "typescript/src"))
(treesit-language-available-p 'tsx)
(treesit-parser-list)
