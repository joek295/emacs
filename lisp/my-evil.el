(use-package evil-leader
             :ensure evil-leader
             :demand evil-leader
             :init
             (global-evil-leader-mode t)
             :config (progn
                       (evil-leader/set-key
                         "f" 'fill-paragraph
                         "n" 'forward-paragraph
                         "p" 'backward-paragraph
                         "j" 'open-line
                         "o" 'newline-below
                         "O" 'newline-above
                         ;; Modes:
                         "d" 'dired
                         "b" 'ibuffer
                         "g" 'magit-status
                         "#" 'comment-or-uncomment-region)
                       )
             )

(use-package evil
             :ensure evil
             :demand evil
             :config (progn
                       (evil-mode 1)
                       )
             )


(defun evil-yank-to-eol ()
  "Yank to end-of-line in evil mode.  Useful for making the
  behaviour of Y sane, i.e. analagous to D, C etc., and not the
  same as yy."
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defadvice evil-goto-line (after evil-goto-line-and-center activate)
  "When using the goto line command ('G'), try to center the line"
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice evil-ex-search-next (after evil-find-key-and-center activate)
  "When using the search next command ('n'), try to center the line"
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice switch-to-buffer (before return-to-normal-now activate)
  "Return to normal mode before switching buffer"
  (when evil-mode
    (evil-normal-state)))

(defadvice other-window (before other-window-normal-now activate)
  "Return to normal mode before switching window"
  (when evil-mode
    (evil-normal-state)))

; Evil mode keys
(eval-after-load 'evil (progn
                         (define-key evil-normal-state-map (kbd "<left>") 'switch-to-prev-buffer)
                         (define-key evil-normal-state-map (kbd "<right>") 'switch-to-next-buffer)
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
  "h" 'my-dired-up-directory
  "q" 'kill-this-buffer
  "n" 'evil-search-next
  "N" 'evil-search-previous
  )

(evil-define-key 'normal ibuffer-mode-map
  "j" 'ibuffer-forward-line
  "k" 'ibuffer-backward-line
  "l" 'ibuffer-visit-buffer
  "U" 'ibuffer-unmark-backward
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
  "s" 'magit-stage-item
  "S" 'magit-stage-all
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
