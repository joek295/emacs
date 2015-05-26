;; load solarized
(set-frame-parameter nil 'background-mode 'dark)
(setq visible-bell nil)

(load-theme 'solarized t)

;; customize fancy-modeline for solarized
(require 'fancy-modeline)

(global-hl-line-mode 1)
(set-face-background hl-line-face "#073642")

;; customisation for graphical systems
(when (display-graphic-p)
  (progn
    (set-face-attribute 'default nil :height 100)
    (set-face-attribute 'mode-line-standard-face nil
                        :foreground "base03" :background "#e4e4e4")
    (set-face-attribute 'mode-line-bold-face nil
                        :foreground "base03" :background "#eee8d5")
    (set-face-attribute 'mode-line-position-face nil
                        :foreground "#eee8d5" :background "#073642")
    (set-face-attribute 'mode-line-insert-face nil
                        :foreground "#dc322f" :background "base03")
    (set-face-attribute 'mode-line-normal-face nil
                        :foreground "#268bd2" :background "base03")
    (set-face-attribute 'mode-line-emacs-face nil
                        :foreground "#cb4b16" :background "base03")
    (set-face-attribute 'mode-line-visual-face nil
                        :foreground "#859900" :background "base03")
    (set-face-attribute 'mode-line-modified-face nil
                        :foreground "#d33682" :background "#073642")
    (set-face-attribute 'mode-line-readonly-face nil
                        :foreground "#6c71c4" :background "#073642")
    )
  )

(progn
  (set-face-attribute 'mode-line-inactive nil :foreground "base03")
  (set-face-attribute 'mode-line nil :foreground "#eee8d5")
  )

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
