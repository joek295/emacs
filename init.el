;;; Joe Kitchen's init.el

;; Package management
(require 'package) 
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/")) 
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")) 
(package-initialize)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/fancy-modeline")

(autoload 'fortunate-mode "fortunate")
(load "functions.el")
(load "keys.el")
(load "my-config.el")
(load "modes.el")
; M-x customize saves its configurations in the file 'custom-file'
(setq custom-file "~/.emacs.d/my-custom.el")
(load custom-file)

;;; end
