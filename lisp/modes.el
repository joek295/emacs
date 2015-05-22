;;; Mode Configuration

;; Loading and configuration of modes goes in this file.  This
;; includes mode-hooks for loading minor modes, advice for mode
;; functions, and suchlike.

; load the needed mode files
(require 'color-theme)
(require 'color-theme-solarized)
(require 'fancy-modeline)
(require 'ido)
(require 'evil)
(require 'evil-leader)
(autoload 'rainbow-delimiters-mode "rainbow-delimiters")

(electric-pair-mode)
(column-number-mode 1)
(menu-bar-mode 0)
(global-linum-mode 1)
(ido-mode t)
(global-evil-leader-mode)
(evil-mode 1)
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
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-ignore-buffers '("\\` " "^\*"))
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
                  (call-interactively 'self-insert-command))))))

;; evil mode
(defadvice evil-goto-line (after evil-goto-line-and-center activate)
  "When using the goto line command ('G'), try to center the line"
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice evil-ex-search-next (after evil-find-key-and-center activate)
  "When using the search next command ('n'), try to center the line"
  (evil-scroll-line-to-center (line-number-at-pos)))

(defadvice switch-to-buffer (before return-to-normal-now activate)
  "Return to normal mode before switching buffer"
  (when evil-mode
    (evil-normal-state)))

(defadvice other-window (before other-window-normal-now activate)
  "Return to normal mode before switching window"
  (when evil-mode
    (evil-normal-state)))

;; magit mode
(defadvice magit-status (around magit-fullscreen activate)
  "Open magit-status in fullscreen rather than splitting into two windows."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

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
  (rainbow-delimiters-mode)
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
(add-hook 'magit-log-edit-mode-hook 'refill-mode)

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
