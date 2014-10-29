;;; fancy-modeline: a modeline for evil-mode users

;; Author: Joe Kitchen 
;; Created: 2014-10-27
;; Version: 1.0.0

;; This is free and unencumbered software released into the public
;; domain.

;;; Features:

;; fancy-modeline is a modeline designed for use with evil mode.  it
;; shows the current evil mode state, the active window, the buffer
;; name, the current major mode, the cursor position, and whether a
;; file is modified or read only.  it is designed to be flexible and
;; simple to reconfigure to the user's liking.  the code should be
;; well laid out, and easy to read, understand, and modify.

;;; Code:

(defface mode-line-standard-face `((t (:foreground "white" :background "black"))) "White on black as standard modeline font")
(defface mode-line-bold-face `((t (:foreground "white" :weight bold :background "black"))) "White bold on black for important information")

(defface mode-line-position-face `((t (:foreground "black" :weight bold :background "white"))) "Black on white for position of cursor")

(defface mode-line-insert-face `((t (:foreground "black" :background "red"))) "Red background for insert-state indicator")
(defface mode-line-normal-face `((t (:foreground "black" :background "blue"))) "Blue background for normal-state-indicator")
(defface mode-line-visual-face `((t (:foreground "black" :background "green"))) "Green background for visual-state-indicator")
(defface mode-line-emacs-face `((t (:foreground "black" :background "orange"))) "Orange background for emacs-state-indicator")

(defface mode-line-active-face `((t (:foreground "black" :background "cyan"))) "Black on cyan for active window")
(defface mode-line-modified-face `((t (:foreground "black" :weight bold :background "magenta"))) "Black on magenta for modified indicator")
(defface mode-line-readonly-face `((t (:foreground "black" :weight bold :background "violet"))) "Black on violet for RO indicator")

(set-face-background 'mode-line "black")

(defun window-active-p ()
  "Return true if window is active.

Code taken from /u/ijustwantanfingname on /r/emacs.

http://reddit.com/r/emacs/2hhhvg"
  (eq
   (frame-selected-window)
   (get-buffer-window)))

(setq my-mode-line-position
      '(:eval (concat (propertize "[%p: L%02l, C%02c]" 'face 'mode-line-position-face)
                      (propertize "|" 'face 'mode-line-standard-face))))

(setq my-mode-line-readonly
      '(:eval (when buffer-read-only
                (propertize "[ RO ]" 'face 'mode-line-readonly-face))))

(setq my-mode-line-modified
      '(:eval (when (buffer-modified-p)
                (propertize "[ + ]" 'face 'mode-line-modified-face))))

(setq my-mode-line-modename
      '(:eval (concat (propertize " %m " 'face 'mode-line-bold-face)
                      (propertize "|" 'face 'mode-line-standard-face))))

(setq my-mode-line-buffname
      '(:eval (concat (propertize " %b " 'face 'mode-line-bold-face)
                      (propertize "|" 'face 'mode-line-standard-face))))

(setq my-mode-line-evilstate
      '(:eval (cond ((evil-insert-state-p) (concat (propertize " INSERT " 'face 'mode-line-insert-face)
                                                   (propertize "|" 'face 'mode-line-standard-face)))
                    ((evil-normal-state-p) (concat (propertize " NORMAL " 'face 'mode-line-normal-face)
                                                   (propertize "|" 'face 'mode-line-standard-face)))
                    ((evil-emacs-state-p) (concat (propertize "  EMACS " 'face 'mode-line-emacs-face)
                                                  (propertize "|" 'face 'mode-line-standard-face)))
                    ((evil-replace-state-p) (concat (propertize " REPLACE " 'face 'mode-line-insert-face)
                                                    (propertize "|" 'face 'mode-line-standard-face)))
                    ((evil-visual-state-p) (concat (propertize " VISUAL " 'face 'mode-line-visual-face)
                                                   (propertize "|" 'face 'mode-line-standard-face))))))

(setq my-mode-line-active
      '(:eval (when (window-active-p)
                (concat (propertize "ACTIVE" 'face 'mode-line-active-face)
                        (propertize "|" 'face 'mode-line-standard-face)))))

(setq-default mode-line-format
              (list
               my-mode-line-evilstate
               my-mode-line-active
               my-mode-line-buffname
               my-mode-line-separator
               my-mode-line-modename
               my-mode-line-separator
               my-mode-line-position
               my-mode-line-modified
               my-mode-line-readonly
               ))

(provide 'fancy-modeline)

;;; END
