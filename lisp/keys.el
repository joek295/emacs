;;; Key bindings

; This file contains personally defined keybindings.  In theory, all of
; the keys <C-c ?>, where ? is a single letter or number are free, as
; are all Fn where n > 4

; evil mode and evil leader mode keys should only load if the
; necessary mode has initialised

;; Universal keys:

; Overriding built-in functionality:
(global-set-key (kbd "C-x C-e") 'eval-defun)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x k") 'kill-active-buffer)

; C-c <some key>
(global-set-key (kbd "C-c e") 'eval-buffer)
(global-set-key (kbd "C-c C-e") 'eval-last-sexp)
(global-set-key (kbd "C-c E") 'fc-eval-and-replace)
(global-set-key (kbd "C-c b") (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "C-c v") 'split-window-right-and-switch)
(global-set-key (kbd "C-c f") 'elfeed)

                                        ; F<n>
(global-set-key (kbd "<f1>") 'quickswitch)
(global-set-key (kbd "<f2>") (lambda () (interactive) (setq quickbuffer (buffer-name))))
(global-set-key (kbd "<f12>") 'my-toggle-input-method)

;; Mode specific keys:

; Evil leader keys

; Elfeed
(defun my-elfeed-search-keys ()
  "Modify the default elfeed keymap"
  (local-set-key "RET" 'elfeed-browse-url))
