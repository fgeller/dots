(use-package markdown-mode 
  :ensure t
  :ensure markdown-preview-mode
  :mode
  ("\\md\\'" . markdown-mode)
  :config
  (setq 
   markdown-command "multimarkdown"
   markdown-preview-stylesheets (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
									  "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css" "
  <style>
   .markdown-body {
     box-sizing: border-box;
     min-width: 200px;
     max-width: 980px;
     margin: 0 auto;
     padding: 45px;
   }

   @media (max-width: 767px) {
     .markdown-body {
       padding: 15px;
     }
   }
  </style>
")
  
   markdown-preview-javascript (list "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js" "
  <script>
   $(document).on('mdContentChange', function() {
     $('pre code').each(function(i, block) {
       hljs.highlightBlock(block);
     });
   });
  </script>
"))

  (defun fg/markdown-mode-hooks ()
	(lspce-mode))

  (add-hook 'markdown-mode-hook 'fg/markdown-mode-hook)
)

