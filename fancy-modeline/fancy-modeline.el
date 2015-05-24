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

(defface mode-line-standard-face
  `((t (:foreground "white" :background "black")))
  "White on black as standard modeline font")
(defface mode-line-bold-face
  `((t (:foreground "white" :weight bold :background "black")))
  "White bold on black for important information")
(defface mode-line-position-face
  `((t (:foreground "black" :weight bold :background "white")))
  "Black on white for position of cursor")
(defface mode-line-insert-face
  `((t (:foreground "black" :background "red")))
  "Red background for insert-state indicator")
(defface mode-line-normal-face
  `((t (:foreground "black" :background "blue")))
  "Blue background for normal-state-indicator")
(defface mode-line-visual-face
  `((t (:foreground "black" :background "green")))
  "Green background for visual-state-indicator")
(defface mode-line-emacs-face
  `((t (:foreground "black" :background "orange")))
  "Orange background for emacs-state-indicator")
(defface mode-line-active-face
  `((t (:foreground "black" :background "cyan")))
  "Black on cyan for active window")
(defface mode-line-modified-face
  `((t (:foreground "black" :weight bold :background "magenta")))
  "Black on magenta for modified indicator")
(defface mode-line-readonly-face
  `((t (:foreground "black" :weight bold :background "violet")))
  "Black on violet for RO indicator")

(set-face-background 'mode-line "white")
(set-face-background 'mode-line-inactive "black")

(defun window-active-p ()
  "Return true if window is active.

Code taken from /u/ijustwantanfingname on /r/emacs.

http://reddit.com/r/emacs/2hhhvg"
  (eq
   (selected-window)
   (get-buffer-window)))

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
      '(:eval (cond ((evil-insert-state-p)  (propertize " I " 'face 'mode-line-insert-face))
                    ((evil-normal-state-p)  (propertize " N " 'face 'mode-line-normal-face))
                    ((evil-motion-state-p)  (propertize " M " 'face 'mode-line-normal-face))
                    ((evil-emacs-state-p)   (propertize " E " 'face 'mode-line-emacs-face))
                    ((evil-replace-state-p) (propertize " R " 'face 'mode-line-insert-face))
                    ((evil-visual-state-p)  (propertize " V " 'face 'mode-line-visual-face))
                    )))

(setq my-mode-line-overwrite
      '(:eval (if overwrite-mode
                  (propertize "OVR" 'face 'mode-line-emacs-face)
                (propertize "INS" 'face 'mode-line-standard-face))))


(setq my-mode-line-active
; this is extremely buggy and doesn't work at all consistently
      '(:eval (when (window-active-p)
                (propertize " A " 'face 'mode-line-active-face))))

(setq my-mode-line-input
; this is extremely buggy and doesn't work at all consistently
     '(:eval (propertize (message current-input-method) 'face 'mode-line-standard-face)))

(setq-default mode-line-format
              (list
               my-mode-line-evilstate
               my-mode-line-buffname
               my-mode-line-modename
               my-mode-line-position
               my-mode-line-modified
               my-mode-line-readonly
               my-mode-line-overwrite
;               my-mode-line-active
;               my-mode-line-input
               ))

(provide 'fancy-modeline)

;;; END
