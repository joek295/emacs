;;; Helm

(use-package helm
  :ensure helm
  :config
  (progn
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-h a") 'helm-apropos)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-x b") 'helm-buffers-list)
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-h") 'helm-find-files-up-one-level)
    (define-key helm-map (kbd "C-l") (kbd "RET"))
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-map [escape] 'helm-keyboard-quit)
    )
  )
