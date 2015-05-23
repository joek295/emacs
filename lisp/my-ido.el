;; ido mode configuration
(use-package ido
  :config (progn
            (ido-mode t)
            (ido-everywhere t)
            (setq ido-enable-flex-matching t)
            (setq ido-ignore-files)
            (add-to-list 'ido-ignore-files "\.pdf")
            (defun ido-ignore-non-user-except (name)
              "Ignore all non-user (*starred*) buffers except certain ones."
              (and (string-match "^\*" name)
                   (not (string= name "*magit-edit-log*"))))
            (setq ido-ignore-buffers '("\\` " ido-ignore-non-user-except))
            (setq ido-create-new-buffer 'always)
            (defun my-ido-jump-to-home ()
              "Jump to the home directory in ido."
              (interactive)
              (ido-set-current-directory "~/")
              (setq ido-exit 'refresh)
              (exit-minibuffer)
              )
            (defun my-setup-ido ()
              "Setup ido right.  When typing '~', go to home."
              (define-key ido-file-completion-map "~" 'my-ido-jump-to-home))
            (add-hook 'ido-setup-hook 'my-setup-ido)
            )
  )
