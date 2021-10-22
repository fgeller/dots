(install 'dap-mode)

;(dap-go-setup)

(after 'go-mode
  (require 'dap-go))

(after 'dap-mode
  (dap-register-debug-template
   "Debug refbd"
   (list :type "go"
         :request "launch"
         :name "Debug refbd"
         :mode "debug"
         :program nil
         :buildFlags nil
         :args "/home/fgeller/src/gitlab.com/refurbed/platform/misc/refbd.conf.local.yaml"
         :env nil
         :envFile "/home/fgeller/src/gitlab.com/refurbed/platform/modd.env"))
  
  (dap-register-debug-template "go test"
                               (list :type "go"
                                     :request "launch"
                                     :name "launch go test"
                                     :mode "auto"
                                     :program nil
                                     :buildFlags nil
                                     :args nil
                                     :env nil
                                     :envFile nil))
  
  (dap-register-debug-template "go test --tags=integration_test"
                               (list :type "go"
                                     :request "launch"
                                     :name "launch go integration test"
                                     :mode "auto"
                                     :program nil
                                     :buildFlags  (list "-tags=integration_test")
                                     :args nil
                                     :env nil
                                     :envFile "/home/fgeller/src/gitlab.com/refurbed/platform/int-test.env"))


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

