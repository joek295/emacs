;;; Mode Configuration

;; Loading and configuration of modes goes in this file.  This
;; includes mode-hooks for loading minor modes, advice for mode
;; functions, and suchlike.

; load the needed mode files
(require 'color-theme)
(require 'color-theme-solarized)
(require 'fancy-modeline)
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

(electric-pair-mode 1)
(column-number-mode 1)
(menu-bar-mode 0)
(global-linum-mode 1)
(auto-compression-mode 1)


(global-hl-line-mode 1)
(set-face-background hl-line-face "gray20")

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
  )
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
