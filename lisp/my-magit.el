;; Magit configuration
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
      "j" 'magit-goto-next-section
      "k" 'magit-goto-previous-section
      "TAB" 'magit-toggle-section
      "g" 'magit-refresh
      "G" 'magit-refresh-all
      "s" 'magit-stage-item
      "S" 'magit-stage-all
      "i" 'magit-ignore-item
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
      "q" 'magit-mode-quit-window
      "d" 'magit-diff-working-tree
      "D" 'magit-diff)
    (set-face-attribute 'magit-section-title nil
                        :foreground "black")
    ))
