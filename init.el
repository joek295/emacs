;;; Joe Kitchen's init.el

;; Package management
(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")) 
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/fancy-modeline")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(setq Info-additional-directory-list '("/home/joe/info"))

(autoload 'fortunate-mode "fortunate")
(autoload 'shortlines-mode "shortlines")
(autoload 'viml-mode "viml")
(autoload 'xbindkeys-mode "xbindkeys")
(autoload 'brainfuck-mode "brainfuck")
(autoload 'nethackrc-mode "nh-config")
(load "my-evil.el")
(load "my-magit.el")
(load "my-elfeed.el")
(load "my-ido.el")
(load "modes.el")
(load "functions.el")
(load "my-helm.el")
(load "keys.el")
(load "my-config.el")
(load "my-eyecandy.el")

; M-x customize saves its configurations in the file 'custom-file'
(setq custom-file "~/.emacs.d/lisp/my-custom.el")
(load custom-file)

;;; end
