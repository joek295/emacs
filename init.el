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

(autoload 'fortunate-mode "fortunate")
(autoload 'shortlines-mode "shortlines")
(autoload 'viml-mode "viml")
(autoload 'xbindkeys-mode "xbindkeys")
(autoload 'nethackrc-mode "nh-config")
(load "my-evil.el")
(load "my-magit.el")
(load "my-ido.el")
(load "modes.el")
(load "functions.el")
(load "keys.el")
(load "my-config.el")
(load "my-eyecandy.el")

; M-x customize saves its configurations in the file 'custom-file'
(setq custom-file "~/.emacs.d/lisp/my-custom.el")
(load custom-file)

(put 'dired-find-alternate-file 'disabled nil)
;;; end
(put 'narrow-to-region 'disabled nil)
