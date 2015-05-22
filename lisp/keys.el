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
(eval-after-load 'evil-leader
  (evil-leader/set-key
    "f" 'fill-paragraph
    "n" 'forward-paragraph
    "p" 'backward-paragraph
    "j" 'open-line
    "o" 'newline-below
    "O" 'newline-above
    "d" 'dired
    "g" 'magit-status
    ;; "i" 'latex-emphasise-word
    "#" 'comment-or-uncomment-region)
  )

; Evil mode keys
(eval-after-load 'evil (progn
                         (define-key evil-normal-state-map "L" 'evil-end-of-line)
                         (define-key evil-normal-state-map "H" 'smart-move-to-beginning-of-line)
                         (define-key evil-normal-state-map "Y" 'evil-yank-to-eol)
                         (define-key evil-normal-state-map "zg" 'my-save-word)
                         (define-key evil-normal-state-map "]s" 'flyspell-goto-next-error)
                         (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
                         (define-key evil-visual-state-map (kbd "C-g") 'evil-normal-state)
                         (define-key evil-replace-state-map (kbd "C-g") 'evil-normal-state)
                         ))

; Mode-specific evil keymaps

(evil-define-key 'normal prog-mode-map
  (kbd "C-c i") 'indent-buffer
  )

(evil-define-key 'visual prog-mode-map
  (kbd "C-c i") 'indent-region
  )

(evil-define-key 'normal org-mode-map
  (kbd "TAB") 'org-cycle
  )

(evil-define-key 'normal eww-mode-map
  "q" 'kill-active-buffer
  (kbd "DEL") 'eww-back-url
  )

(evil-define-key 'normal dired-mode-map
  "l" 'dired-find-alternate-file
  "h" 'dired-up-directory
  "q" 'kill-this-buffer
  )

(evil-define-key 'normal ibuffer-mode-map
  "j" 'ibuffer-forward-line
  "k" 'ibuffer-backward-line
  "l" 'ibuffer-visit-buffer
  "/" 'ibuffer-jump-to-buffer
  )

(evil-define-key 'emacs ibuffer-mode-map
  "j" 'ibuffer-forward-line
  "k" 'ibuffer-backward-line
  "l" 'ibuffer-visit-buffer
  "/" 'ibuffer-jump-to-buffer
  )

(evil-define-key 'normal magit-mode-map
  "j" 'magit-goto-next-section
  "k" 'magit-goto-previous-section
  "TAB" 'magit-toggle-section
  "g" 'magit-refresh
  "G" 'magit-refresh-all
  "?" 'magit-describe-item
  ":" 'magit-git-command
  "RET" 'magit-visit-item
  "t" 'magit-key-mode-popup-tagging
  "r" 'magit-key-mode-popup-rewriting
  "P" 'magit-key-mode-popup-pushing
  "f" 'magit-key-mode-popup-fetching
  "b" 'magit-key-mode-popup-branching
  "M" 'magit-key-mode-popup-remoting
  "B" 'magit-key-mode-popup-bisecting
  "F" 'magit-key-mode-popup-pulling
  "l" 'magit-key-mode-popup-logging
  "c" 'magit-log-edit
  "q" 'magit-quit-window
  "d" 'magit-diff-working-tree
  "D" 'magit-diff
)

; Elfeed
(defun my-elfeed-search-keys ()
  "Modify the default elfeed keymap"
  (local-set-key "RET" 'elfeed-browse-url))
