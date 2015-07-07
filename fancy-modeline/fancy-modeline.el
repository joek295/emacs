;;; fancy-modeline: a modeline for evil-mode users

;; Author: Joe Kitchen
;; Created: 2014-10-27
;; Version: 1.0.0

;; This is free and unencumbered software released into the public
;; domain.

;;; Features:

;; fancy-modeline is a modeline designed for use with evil mode.  It
;; shows the current evil mode state, the active window, the buffer
;; name, the current major mode, the cursor position, and whether a
;; file is modified or read only.  It is designed to be flexible and
;; simple to reconfigure to the user's liking.  The code should be
;; well laid out, and easy to read, understand, and modify.

;;; Code:

(defface mode-line-standard-face
  `((t (:foreground "#eee8d5" :background "#002b36")))
  "standard modeline font")
(defface mode-line-bold-face
  `((t (:foreground "#eee8d5" :weight bold :background "#002b36")))
  "font for key information such as buffer name")
(defface mode-line-position-face
  `((t (:foreground "#002b36" :weight bold :background "#eee8d5")))
  "font for cursor position information")
(defface mode-line-insert-face
  `((t (:foreground "#002b36" :background "red")))
  "font for mode-line insert flag")
(defface mode-line-normal-face
  `((t (:foreground "#002b36" :background "blue")))
  "font for normal-state-flag")
(defface mode-line-visual-face
  `((t (:foreground "#002b36" :background "green")))
  "font for visual-state-flag")
(defface mode-line-emacs-face
  `((t (:foreground "#002b36" :background "orange")))
  "font for visual-state flag")
(defface mode-line-active-face
  `((t (:foreground "#002b36" :background "cyan")))
  "font for active-state-flag")
(defface mode-line-modified-face
  `((t (:foreground "#002b36" :weight bold :background "magenta")))
  "font for modified flag")
(defface mode-line-readonly-face
  `((t (:foreground "#002b36" :weight bold :background "#6c71c4")))
  "font for read-only flag")

(set-face-foreground 'mode-line "#a9a9a9")
(set-face-foreground 'mode-line-inactive  "#073642")
(set-face-background 'mode-line "#002b36")
(set-face-background 'mode-line-inactive "#eee8d5")

(setq my-mode-line-position
      '(:eval (propertize "[%p: L%02l C%02c]" 'face 'mode-line-position-face)))

(setq my-mode-line-readonly
      '(:eval (when buffer-read-only
                (propertize "[RO]" 'face 'mode-line-readonly-face))))

(setq my-mode-line-modified
      '(:eval (when (buffer-modified-p)
                (propertize "[ + ]" 'face 'mode-line-modified-face))))

(setq my-mode-line-modename
      '(:eval (propertize " %m " 'face 'mode-line-bold-face)))
              

(setq my-mode-line-buffname
      '(:eval (propertize " %b " 'face 'mode-line-bold-face)))

(setq my-mode-line-evilstate
      '(:eval (cond ((evil-insert-state-p)    (propertize " I " 'face 'mode-line-insert-face))
                    ((evil-normal-state-p)    (propertize " N " 'face 'mode-line-normal-face))
                    ((evil-motion-state-p)    (propertize " M " 'face 'mode-line-normal-face))
                    ((evil-operator-state-p)  (propertize " O " 'face 'mode-line-normal-face))
                    ((evil-emacs-state-p)     (propertize " E " 'face 'mode-line-emacs-face))
                    ((evil-replace-state-p)   (propertize " R " 'face 'mode-line-insert-face))
                    ((evil-visual-state-p)    (propertize " V " 'face 'mode-line-visual-face))
                    )))

(setq my-mode-line-overwrite
      '(:eval (if overwrite-mode
                  (propertize "OVR" 'face 'mode-line-emacs-face)
                (propertize "INS" 'face 'mode-line-standard-face))))

(setq my-mode-line-git
      '(vc-mode vc-mode))

(setq my-mode-line-format
              (list
               my-mode-line-evilstate
               my-mode-line-buffname
               my-mode-line-modename
               my-mode-line-position
               my-mode-line-modified
               my-mode-line-readonly
               my-mode-line-overwrite
               my-mode-line-git
               ))

(defun my-reset-modeline ()
  "Load new modeline"
  (interactive)
  (setq mode-line-format my-mode-line-format))

(setq-default mode-line-format my-mode-line-format)

(provide 'fancy-modeline)

;;; END
