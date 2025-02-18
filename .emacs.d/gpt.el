(use-package gptel 
  :ensure t
  :config
  (require 'gptel-transient)
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))
  (setq gptel-api-key (auth-source-pick-first-password
                       :host "api.openai.com"
                       :user "OPENAI_API_KEY"))
)

