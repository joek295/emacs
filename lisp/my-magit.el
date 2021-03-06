;; Magit configuration
(setq magit-last-seen-setup-instructions "1.4.0")
(use-package magit
  :ensure magit
  :config
  (progn
    (evil-set-initial-state 'magit-mode 'normal)
    (evil-set-initial-state 'magit-status-mode 'normal)
    (evil-set-initial-state 'magit-diff-mode 'normal)
    (evil-set-initial-state 'magit-log-mode 'normal)
    ;; Advice taken from what the emacsd!?
    (defadvice magit-status (around magit-fullscreen activate)
      "Open magit-status in fullscreen rather than splitting into two windows."
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    (add-hook 'magit-log-edit-mode-hook 'refill-mode)
    ;; Magit keybindings with evil mode
    (evil-define-key 'normal magit-mode-map
      "j" 'magit-section-forward
      "k" 'magit-section-backward
      (kbd "TAB") 'magit-section-toggle
      "g" 'magit-refresh
      "G" 'magit-refresh-all
      "s" 'magit-stage-item
      "S" 'magit-stage-all
      "i" 'magit-gitignore
      "r" 'magit-revert-item
      "c" 'magit-commit 
      "q" 'kill-active-buffer
      "p" 'magit-push
      (kbd "RET") 'magit-diff-visit-file
      "b" 'magit-key-mode-popup-branching
      "m" 'magit-key-mode-popup-merging
;; I have never used any of the below keybindings...
      "?" 'magit-describe-item
      ":" 'magit-git-command
      "t" 'magit-key-mode-popup-tagging
      "f" 'magit-key-mode-popup-fetching
      "M" 'magit-key-mode-popup-remoting
      "B" 'magit-key-mode-popup-bisecting
      "F" 'magit-key-mode-popup-pulling
      "l" 'magit-key-mode-popup-logging
      "d" 'magit-diff-working-tree
      "D" 'magit-diff)
    ))
