;;; -*- coding:utf-8 -*-
(setq
 init-gc-cons-threshold gc-cons-threshold
 init-file-name-handler-alist file-name-handler-alist
 gc-cons-threshold 100000000
 file-name-handler-alist nil)

(setq mac-p (eq system-type 'darwin))
(setq linux-p (eq system-type 'gnu/linux))
(setq terminal-p (not window-system))
(when mac-p (setq system-name (car (split-string system-name "\\."))))

(defalias 'after 'with-eval-after-load)
(defalias 'file-name 'expand-file-name)

(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")

(setq custom-site-lisp (file-name "~/.emacs.d/site-lisp"))
(add-to-list 'load-path custom-site-lisp)
(let ((default-directory custom-site-lisp)) (normal-top-level-add-subdirs-to-load-path))

(mapcar (lambda (file)
	  (when (file-directory-p file) (add-to-list 'custom-theme-load-path file)))
	(directory-files (file-name (file-name "themes" custom-site-lisp)) t))

(setq-default custom-file (file-name "~/.emacs.d/custom.el"))

(require 'cl)
(require 'package)
(require 'uniquify)
(require 'subword)

(setq package-user-dir (file-name (file-name "elpa" custom-site-lisp)))
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(defun install (package &optional req)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package))
  (when req (require package)))

(when (memq window-system '(mac ns))
  (install 'exec-path-from-shell)
  (let ((exec-path-from-shell-variables '("PATH" "MANPATH" "GPG_AGENT_INFO" "SSH_AUTH_SOCK" "LANG")))
    (exec-path-from-shell-initialize)))

(setq auto-save-default nil)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq auto-revert-interval 0.1)
(auto-revert-set-timer)

(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.01)

(setq scroll-step 1
      scroll-conservatively 10000)

(setq enable-local-variables :all)

(setq-default fill-column 80)

(column-number-mode 1)

(electric-indent-mode 1)
(electric-pair-mode 1)

(setq-default require-final-newline t)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(setq dired-dwim-target t)
(setq dired-listing-switches "-laGh")

(load "~/.emacs.d/ivy.el")
(load "~/.emacs.d/fingers.el")
(load "~/.emacs.d/appearance.el")
(load "~/.emacs.d/yasnippet.el")
(load "~/.emacs.d/org.el")
(load "~/.emacs.d/vc.el")
(load "~/.emacs.d/search.el")
(load "~/.emacs.d/auto-complete.el")
(load "~/.emacs.d/emacs-lisp.el")
(load "~/.emacs.d/scala.el")
(load "~/.emacs.d/go.el")
(load "~/.emacs.d/web-mode.el")
(load "~/.emacs.d/email.el")
(load "~/.emacs.d/compilation.el")
(load "~/.emacs.d/javascript.el")
(load "~/.emacs.d/scratch.el")
(load "~/.emacs.d/flycheck.el")

(setq
 gc-cons-threshold init-gc-cons-threshold
 file-name-handler-alist init-file-name-handler-alist)
