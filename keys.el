;;; Key bindings

; This file contains personally defined keybindings.  In theory, all of
; the keys <C-c ?>, where ? is a single letter or number are free, as
; are all Fn where n > 4

;; Overriding built-in functionality:
(global-set-key (kbd "C-x C-e") 'eval-defun)

;; C-c <some key>
(global-set-key (kbd "C-c e") 'eval-buffer)
(global-set-key (kbd "C-c E") 'fc-eval-and-replace)
(global-set-key (kbd "C-c b") (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "C-c v") 'split-window-right-and-switch)
(global-set-key (kbd "C-c f") 'elfeed)

;; F<n>
(global-set-key (kbd "<f1>") 'quickswitch)
(global-set-key (kbd "<f2>") (lambda () (interactive) (setq quickbuffer (buffer-name))))
(global-set-key (kbd "<f12>") 'my-toggle-input-method)

;; Evil leader keys
(evil-leader/set-key
 "f" 'fill-paragraph
 "n" 'forward-paragraph
 "p" 'backward-paragraph
 "j" 'open-line
 "o" 'newline-below
 "O" 'newline-above
 "d" 'dired
 "g" 'magit-status)

;; only run after evil mode has loaded
(eval-after-load 'evil (progn
                         (define-key evil-normal-state-map "L" 'evil-end-of-line)
                         (define-key evil-normal-state-map "H" 'smart-move-to-beginning-of-line)
                         (define-key evil-insert-state-map "C-g" 'evil-normal-state)
                         (define-key evil-visual-state-map "C-g" 'evil-normal-state)
                         (define-key evil-replace-state-map "C-g" 'evil-normal-state)
                         ))
