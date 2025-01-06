;;; -*- coding:utf-8 -*-

(setq global-startup (current-time)
      time-measured-blocks 0.0)

(defmacro time (msg &rest body)
  `(let ((start-time (current-time)) elapsed)
     ,@body
     (setq elapsed (time-subtract (current-time) start-time))
     (setq time-measured-blocks (+ time-measured-blocks (float-time elapsed)))
     (message "%sms to %s" (format-time-string "%3N" elapsed) ,msg)))
(put 'time 'lisp-indent-function 1)

(defalias 'after 'with-eval-after-load)
(defalias 'file-name 'expand-file-name)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq mac-p (eq system-type 'darwin))
(setq linux-p (eq system-type 'gnu/linux))
(when mac-p (setq system-name (car (split-string system-name "\\."))))

(setenv "EDITOR" "emacs -nw")
(setenv "PAGER" "cat")

(defun add-to-load-path (d)
  (dolist (f (directory-files d nil "[^\\.]$"))
    (add-to-list 'load-path (file-name f d))))

(setq custom-site-lisp (file-name "~/.emacs.d/site-lisp"))
(add-to-list 'load-path custom-site-lisp)
(add-to-load-path custom-site-lisp)

(mapcar (lambda (file)
	      (when (file-directory-p file) (add-to-list 'custom-theme-load-path file)))
	    (directory-files (file-name (file-name "themes" custom-site-lisp)) t))

(require 'uniquify)
(require 'subword)

(setq recentf-max-saved-items 256)

(setq auth-sources '("~/.authinfo.gpg"))

;; (setq package-quickstart t)
(setq package-archives 
	  '(("melpa" . "https://melpa.org/packages/")
		("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
		("gnu" . "https://elpa.gnu.org/packages/")
		("melpa-stable" . "https://stable.melpa.org/packages/")))

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
										(or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
										"--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(use-package esup
  :ensure t
  :pin melpa-stable)

(setq
 auto-save-default nil
 make-backup-files nil
 echo-keystrokes 0.1
 scroll-step 1
 scroll-conservatively 10000
 enable-local-variables :all
 epg-gpg-program "gpg2"
 uniquify-buffer-name-style 'post-forward-angle-brackets
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*"
 dired-dwim-target t
 dired-listing-switches "-laGh"
 initial-major-mode 'fundamental-mode
 initial-scratch-message ""
 warning-minimum-level :emergency)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq-default
 fill-column 80
 tab-width 4
 custom-file (file-name "~/.emacs.d/custom.el")
 require-final-newline t)

(save-place-mode +1)

(column-number-mode 1)
(electric-indent-mode -1)
(electric-pair-mode -1)
(electric-quote-mode -1)
(electric-layout-mode -1)

(defun load-custom (n)
  (time (format "load %s" n)
    (load n nil 'no-message)))

(load-custom "~/.emacs.d/bindings.el")
(load-custom "~/.emacs.d/appearance.el")
(load-custom "~/.emacs.d/mode-line.el")
(load-custom "~/.emacs.d/windows.el")
(load-custom "~/.emacs.d/completion.el")
(load-custom "~/.emacs.d/compilation.el")
(load-custom "~/.emacs.d/search.el")
(load-custom "~/.emacs.d/vc.el")
(load-custom "~/.emacs.d/snippets.el")
(load-custom "~/.emacs.d/prog.el")
(load-custom "~/.emacs.d/lsp.el")
(load-custom "~/.emacs.d/eglot.el")
(load-custom "~/.emacs.d/go.el")
(load-custom "~/.emacs.d/rust.el")
(load-custom "~/.emacs.d/emacs-lisp.el")
(load-custom "~/.emacs.d/org.el")

(load-custom "~/.emacs.d/markdown.el")
(load-custom "~/.emacs.d/html.el")
(load-custom "~/.emacs.d/yaml.el")
(load-custom "~/.emacs.d/typescript.el")
(load-custom "~/.emacs.d/eshell.el")
(load-custom "~/.emacs.d/nix.el")
(load-custom "~/.emacs.d/macos.el")
(load-custom "~/.emacs.d/direnv.el")
(load-custom "~/.emacs.d/dap.el")
(load-custom "~/.emacs.d/term.el")

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq auto-revert-interval 0.5)
(auto-revert-set-timer)
(setq create-lockfiles nil)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(setq file-name-handler-alist init-file-name-handler-alist)

(require 'server)
(unless (server-running-p) (server-start))

(global-auto-revert-mode +1)

(setq
 gc-cons-threshold (* 2 1024 1024)
)

(message "%sms to load init.el (%.00fms measured)" 
		 (format-time-string "%3N" (time-subtract (current-time) global-startup))
		 (* time-measured-blocks 1000.0))
(message "emacs init time: %s" (emacs-init-time))
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
