;;; My Configuration

;; configuration of Emacs which holds for all modes goes here

; remove tool-bar and scroll-bar modes for GUI versions of emacs
(tool-bar-mode -1)
(scroll-bar-mode -1)

; disable start-up messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "joe") 
(setq initial-scratch-message "") 

; tabs
(setq-default tab-width 4
              indent-tabs-mode nil) 

; smoother scrolling
(setq scroll-conservatively 9999
      scroll-step 1) 

; backups
(setq make-backup-files nil) 
(setq auto-save-default nil)

(setq confirm-nonexistent-file-or-buffer nil) 
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

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)

; default directory should be my $HOME directory
(setq default-directory "~/")
