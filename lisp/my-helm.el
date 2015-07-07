;;; Helm

(use-package helm
  :ensure helm
  :config
  (progn
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
    (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
    (define-key helm-find-files-map (kbd "C-j") 'helm-next-line)
    (define-key helm-find-files-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-buffers-list-map (kbd "C-j") 'helm-next-line)
    (define-key helm-buffers-list-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
    (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
    )
  )

