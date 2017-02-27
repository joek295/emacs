; Theme: ample
(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample-flat))
  :defer t
  :ensure t)

;; customisation for graphical systems
; set font and size
(progn
  (set-frame-font "-mlss-Anonymous Pro-*-*-*-*-*-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :height 120)
  )

(setq visible-bell nil)

;; Use fancy-modeline for my modeline
(require 'fancy-modeline)

(global-hl-line-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :config (progn
            (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                                :foreground "red" :weight 'bold)
            (set-face-attribute 'rainbow-delimiters-depth-2-face   nil
                                :foreground "orange" :weight 'bold)
            (set-face-attribute 'rainbow-delimiters-depth-3-face   nil
                                :foreground "yellow" :weight 'bold)
            (set-face-attribute 'rainbow-delimiters-depth-4-face   nil
                                :foreground "green" :weight 'bold)
            (set-face-attribute 'rainbow-delimiters-depth-5-face   nil
                                :foreground "blue" :weight 'bold)
            (set-face-attribute 'rainbow-delimiters-depth-6-face   nil
                                :foreground "color-34" :weight 'bold)
            (set-face-attribute 'rainbow-delimiters-depth-7-face   nil
                                :foreground "color-16" :weight 'bold)
            (set-face-attribute 'rainbow-delimiters-depth-8-face   nil
                                :foreground "white" :weight 'bold)
            (set-face-attribute 'rainbow-delimiters-depth-9-face   nil
                                :foreground "color-81" :weight 'bold)
            (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                                :foreground "magenta" :weight 'extra-bold)
            (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
            ))
