;;; My Configuration

;; configuration of Emacs which holds for all modes goes here

; disable start-up messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "joe") 
(setq initial-scratch-message "") 

; do not show italic face -- it doesn't display
(set-face-italic-p 'italic nil) 

; tabs
(setq-default tab-width 2
              indent-tabs-mode nil) 

; smoother scrolling
(setq scroll-margin 5   
      scroll-conservatively 9999
      scroll-step 1) 

; backups
(setq make-backup-files nil) 
(setq auto-save-default nil)

(setq confirm-nonexistent-file-or-buffer nil) 
(setq visible-bell t) 
(set-display-table-slot standard-display-table 'wrap ?¶) 
(setq default-input-method "TeX") 
(setq fortune-file "/home/joe/fortunes/fortunes")

(defvaralias 'c-basic-offset 'tab-width) 
(defvaralias 'cperl-indent-level 'tab-width) 
(defalias 'yes-or-no-p 'y-or-n-p) 
(defalias 'list-buffers 'ibuffer) 

; navigate windows using Shift-arrow
(when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings))
