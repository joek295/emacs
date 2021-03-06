;;; Mode Configuration

;; Loading and configuration of modes goes in this file.  This
;; includes mode-hooks for loading minor modes, advice for mode
;; functions, and suchlike.

; load the needed mode files

(electric-pair-mode 1)
(column-number-mode 1)
(menu-bar-mode 0)
(global-linum-mode 1)
(auto-compression-mode 1)
;(autoload 'mediawiki)

; which major mode to load:
; text-mode should be default; the .vimrc file should open in
; viml-mode; all .tex files should open in latex-mode; all files
; ending in "fortunes" should open in fortunate-mode
(setq default-major-mode 'text-mode)
(progn
  (setq auto-mode-alist (cons '("\\fortunes$" . fortunate-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.vimrc$" . viml-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.nethackrc$" . nethackrc-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.xbindkeysrc$" . xbindkeys-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.wiki$" . mediawiki-mode) auto-mode-alist))
  ; I use the plugin It's All Text! with Firefox.  When editing
  ; Wikipedia articles with It's All Text, Emacs should automatically
  ; enter mediawiki-mode so we get syntax highlighting.
  (setq auto-mode-alist (cons '("\\.wikipedia\\.org" . mediawiki-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.reddit\\.com.*\\.txt$" . markdown-mode) auto-mode-alist))
  )

;; dired mode
(setq dired-listing-switches "--group-directories-first -alh")

;; flyspell mode
(defun my-save-word ()
  "Tell Flyspell that it should remember a word as correct."
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (car (cdr (cdr word))) current-location))))

;; linum mode
(setq linum-format 'dynamic)

;; paredit mode
(use-package paredit
  :ensure t
  :config
  (defun my-paredit-nonlisp ()
    "Turn on paredit mode for non-lisps."
    (interactive)
    (set (make-local-variable 'paredit-space-for-delimiter-predicates)
         '((lambda (endp delimiter) nil)))
    (paredit-mode 1))
  )

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

(defun my-python-mode-hook ()
  "Run when entering python-mode"
  (my-paredit-nonlisp)
  )
(add-hook 'python-mode-hook 'my-python-mode-hook)

; org mode
(defun my-org-mode-hook ()
  "Run when entering org-mode"
  (flyspell-mode)
  (toggle-truncate-lines -1))
(add-hook 'org-mode-hook 'my-org-mode-hook)

; text mode
(defun my-text-mode-hook ()
  "Run when entering text mode"
  (flyspell-mode)
  (visual-line-mode)
  )
(add-hook 'text-mode-hook 'my-text-mode-hook)

; tex mode
(defun my-tex-mode-hook ()
  "TeX mode hook"
  (set-input-method "british")
  )
(add-hook 'tex-mode-hook 'my-tex-mode-hook)

(defun my-mediawiki-mode-hook ()
  "MediaWiki mode hook"
;  (setq sentence-end-double-space nil)
;  (setq sentence-end "[.?!>}][[]\"')}]*\\($\\| \\|\t\\)*")
  (set-input-method "TeX"))
(add-hook 'mediawiki-mode-hook 'my-mediawiki-mode-hook)

; lisp modes
(defun my-lisp-mode-hook ()
  "Run when entering lisp-interaction-mode or emacs-lisp-mode"
  (eldoc-mode)
  (paredit-mode)
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("<=" . ?≤) prettify-symbols-alist)
  (push '("==" . ?≡) prettify-symbols-alist)
  (prettify-symbols-mode))

(add-hook 'lisp-interaction-mode-hook 'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)

; misc modes
(defun my-dired-mode-hook ()
  (dired-omit-mode t)
  )
(add-hook 'dired-mode-hook 'my-dired-mode-hook)

(defun my-eww-mode-hook ()
  (linum-mode -1)
  )

(add-hook 'eww-mode-hook 'my-eww-mode-hook)

(defun my-fortunate-mode-hook ()
  (setq fill-column 72)
  (turn-on-auto-fill)
  )

(add-hook 'fortunate-mode-hook 'my-fortunate-mode-hook)
