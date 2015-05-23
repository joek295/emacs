;;; Joe Kitchen's init.el

;; Package management
(require 'package) 
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/")) 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")) 
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")) 
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
(load "my-evil.el")
(load "functions.el")
(load "keys.el")
(load "my-config.el")
(load "modes.el")
; M-x customize saves its configurations in the file 'custom-file'
(setq custom-file "~/.emacs.d/lisp/my-custom.el")
(load custom-file)

;;; end
(put 'dired-find-alternate-file 'disabled nil)
