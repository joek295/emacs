;;; Mode Configuration

;; Loading and configuration of modes goes in this file.  This
;; includes mode-hooks for loading minor modes, advice for mode
;; functions, and suchlike.

; load the needed mode files
(require 'color-theme)
(require 'color-theme-solarized)
(require 'fancy-modeline)
(require 'ido)
(use-package magit
             :ensure magit
             :config
             (progn
               (evil-set-initial-state 'magit-mode 'normal)
               (evil-set-initial-state 'magit-status-mode 'normal)
               (evil-set-initial-state 'magit-diff-mode 'normal)
               (evil-set-initial-state 'magit-log-mode 'normal)
               ;; Advice taken from what the emacsd!?
               (defadvice magit-status (around magit-fullscreen activate)
                 "Open magit-status in fullscreen rather than splitting into two windows."
                 (window-configuration-to-register :magit-fullscreen)
                 ad-do-it
                 (delete-other-windows))
                (add-hook 'magit-log-edit-mode-hook 'refill-mode)
               ;; Magit keybindings with evil mode
               (evil-define-key 'normal magit-mode-map
                 "j" 'magit-goto-next-section
                 "k" 'magit-goto-previous-section
                 "TAB" 'magit-toggle-section
                 "g" 'magit-refresh
                 "G" 'magit-refresh-all
                 "s" 'magit-stage-item
                 "S" 'magit-stage-all
                 "i" 'magit-ignore-item
                 "?" 'magit-describe-item
                 ":" 'magit-git-command
                 "RET" 'magit-visit-item
                 "t" 'magit-key-mode-popup-tagging
                 "r" 'magit-key-mode-popup-rewriting
                 "P" 'magit-key-mode-popup-pushing
                 "f" 'magit-key-mode-popup-fetching
                 "b" 'magit-key-mode-popup-branching
                 "M" 'magit-key-mode-popup-remoting
                 "B" 'magit-key-mode-popup-bisecting
                 "F" 'magit-key-mode-popup-pulling
                 "l" 'magit-key-mode-popup-logging
                 "c" 'magit-log-edit
                 "q" 'magit-quit-window
                 "d" 'magit-diff-working-tree
                 "D" 'magit-diff)
               ))

(use-package rainbow-delimiters
  :ensure t
  :config (progn
            (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
            ))

(electric-pair-mode 1)
(column-number-mode 1)
(menu-bar-mode 0)
(global-linum-mode 1)
(auto-compression-mode 1)

; which major mode to load:
; text-mode should be default; the .vimrc file should open in
; viml-mode; all .tex files should open in latex-mode; all files
; ending in "fortunes" should open in fortunate-mode
(setq default-major-mode 'text-mode)
(setq auto-mode-alist (cons '("\\fortunes$" . fortunate-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\NEWS$" . shortlines-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\README" . shortlines-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\Readme" . shortlines-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\readme" . shortlines-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.vimrc$" . viml-mode) auto-mode-alist))

;; ido mode
(use-package ido
  :config (progn
            (ido-mode t)
            (ido-everywhere t)
            (setq ido-enable-flex-matching t)
            (setq ido-ignore-files)
            (add-to-list 'ido-ignore-files "\.pdf")
            (defun ido-ignore-non-user-except (name)
              "Ignore all non-user (*starred*) buffers except certain ones."
              (and (string-match "^\*" name)
                   (not (string= name "*magit-edit-log*"))))
            (setq ido-ignore-buffers '("\\` " ido-ignore-non-user-except))
            (setq ido-create-new-buffer 'always)
            (add-hook 'ido-setup-hook
                      (lambda ()
                        "When typing '~' in ido-find-file, go to the home directory."
                        (define-key ido-file-completion-map
                          (kbd "~")
                          (lambda ()
                            (interactive)
                            (if (looking-back "/")
                                (insert "~/")
                              (call-interactively 'self-insert-command))))))))



;; flyspell mode
(defun my-save-word ()
  "Tell Flyspell that it should remember a word as correct."
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

;; linum mode
(setq linum-format 'dynamic)

;; elfeed mode
(setq elfeed-feeds
      '("http://www.terminally-incoherent.com/blog/feed/"
        "http://kmandla.wordpress.com/feed/"
        "http://debianjoe.wordpress.com/feed/"
        "http://inconsolation.wordpress.com/feed/"
        "http://foodthroughthepages.wordpress.com/feed/"
        "http://thepaternaldrunk.com/feed/"
        "http://pervocracy.blogspot.com/feeds/posts/default"
        "http://captainawkward.com/feed/"
        "http://elsiewrites.wordpress.com/feed/"
        "http://www.atomicnerds.com/?feed=rss2"
        "http://adventuresinolomouc.blogspot.com/feeds/posts/default?alt=rss"
        "http://samreadsatlasshrugged.wordpress.com/feed/"
        "http://sexgeek.wordpress.com/feed"
        "http://feeds.feedburner.com/PagingDrNerdlove"))

;; (add-hook 'elfeed-new-entry-hook
;;           (elfeed-make-tagger :entry-title '("Open Thread")
;;                               :remove 'unread)
;;           (elfeed-make-tagger :entry-title '("Meetup")
;;                               :remove 'unread)
;;           )

;; mode hooks
; after-init-hook
(defun my-after-init-hook ()
  "Run after init."
  )
;(add-hook 'after-init-hook 'my-after-init-hook)

; prog mode
(defun my-prog-mode-hook ()
  "Run when entering any mode inheriting from prog-mode"
  (flyspell-prog-mode)
  (hl-line-mode))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

; org mode
(defun my-org-mode-hook ()
  "Run when entering org-mode"
  (flyspell-mode)
  (toggle-truncate-lines -1))
(add-hook 'org-mode-hook 'my-org-mode-hook)

; text mode
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

; tex mode
(add-hook 'tex-mode-hook (lambda () (set-input-method "british")))

; magit modes
(add-hook 'magit-status-mode-hook (lambda () (evil-local-mode 1)))

; lisp modes
(defun my-lisp-mode-hook ()
  "Run when entering lisp-interaction-mode or emacs-lisp-mode"
  (eldoc-mode)
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("<=" . ?≤) prettify-symbols-alist)
  (push '("==" . ?≡) prettify-symbols-alist)
  (prettify-symbols-mode))

(add-hook 'lisp-interaction-mode-hook 'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)

; misc modes
(add-hook 'calendar-mode-hook (lambda () (evil-local-mode -1)))
(add-hook 'eww-mode-hook (lambda () (linum-mode -1)))
(add-hook 'elfeed-mode-hook 'my-elfeed-search-keys)
