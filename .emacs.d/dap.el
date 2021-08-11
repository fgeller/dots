(install 'dap-mode)

;; (dap-go-setup)

(after 'go-mode
  (require 'dap-go))

(after 'dap-mode
  (dap-register-debug-template
   "Debug refbapid"
   (list :type "go"
         :request "launch"
         :name "Debug refbapid"
         :mode "debug"
         :program nil
         :buildFlags nil
         :args nil
         :env nil
         :envFile "/home/fgeller/src/gitlab.com/refurbed/platform/modd.env")))
