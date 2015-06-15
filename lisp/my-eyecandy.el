;; load solarized
(set-frame-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

(setq visible-bell nil)

;; Use fancy-modeline for my modeline
(require 'fancy-modeline)

(global-hl-line-mode 1)

;; customisation for graphical systems
(progn
  (set-face-attribute 'default nil :height 100)
  )

(progn
  (set-face-attribute 'linum nil :foreground "white" :background "base03")
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

(defun my-fancy-modeline-fix-colors ()
  "Set fancy-modeline colours correctly.  First check whether we
  are running emacs in a terminal, and then set the colours
  appropriately."
  (interactive)
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'mode-line-standard-face nil
                            :foreground "#073642" :background "#e4e4e4")
        (set-face-attribute 'mode-line-bold-face nil
                            :foreground "#073642" :background "#eee8d5")
        (set-face-attribute 'mode-line-position-face nil
                            :foreground "#eee8d5" :background "#073642")
        (set-face-attribute 'mode-line-insert-face nil
                            :foreground "#dc322f" :background "#073642")
        (set-face-attribute 'mode-line-normal-face nil
                            :foreground "#268bd2" :background "#073642")
        (set-face-attribute 'mode-line-emacs-face nil
                            :foreground "#cb4b16" :background "#073642")
        (set-face-attribute 'mode-line-visual-face nil
                            :foreground "#859900" :background "#073642")
        (set-face-attribute 'mode-line-modified-face nil
                            :foreground "#d33682" :background "#073642")
        (set-face-attribute 'mode-line-readonly-face nil
                            :foreground "#6c71c4" :background "#073642"))
    (progn
      (set-face-attribute 'mode-line-standard-face nil
                          :foreground "#4d4d4d" :background "#e4e4e4")
      (set-face-attribute 'mode-line-bold-face nil
                          :foreground "#4d4d4d" :background "#eee8d5")
      (set-face-attribute 'mode-line-position-face nil
                          :foreground "#eee8d5" :background "#4d4d4d")
      (set-face-attribute 'mode-line-insert-face nil
                          :foreground "#dc322f" :background "#4d4d4d")
      (set-face-attribute 'mode-line-normal-face nil
                          :foreground "#268bd2" :background "#4d4d4d")
      (set-face-attribute 'mode-line-emacs-face nil
                          :foreground "#cb4b16" :background "#4d4d4d")
      (set-face-attribute 'mode-line-visual-face nil
                          :foreground "#859900" :background "#4d4d4d")
      (set-face-attribute 'mode-line-modified-face nil
                          :foreground "#d33682" :background "#4d4d4d")
      (set-face-attribute 'mode-line-readonly-face nil
                          :foreground "#6c71c4" :background "#4d4d4d")
      )))

(defun my-fix-colors ()
  "Setup colors correctly for graphical and terminal emulator
versions of emacs.  Ensure that the modeline looks correct, and
that hl-line-mode's line highlighting looks correct."
  (interactive)
  (my-fancy-modeline-fix-colors)
  (if (display-graphic-p)
      (progn
        (set-face-background 'hl-line "#073642")
        (set-face-attribute 'mode-line-inactive nil :foreground "#073642")
        (set-face-attribute 'mode-line nil :foreground "#a9a9a9")
        )
    (progn
      (set-face-background 'hl-line "#000000"))
    (set-face-attribute 'mode-line-inactive nil :foreground "#4d4d4d")
    (set-face-attribute 'mode-line nil :foreground "#e4e4e4")
    )
  )

(my-fix-colors)
(global-set-key (kbd "<f3>") 'my-fix-colors)
