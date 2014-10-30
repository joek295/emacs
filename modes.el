;;; Mode Configuration

;; Loading and configuration of modes goes in this file.  This
;; includes mode-hooks for loading minor modes, advice for mode
;; functions, and suchlike.

; fix evil mode so that <tab> works as expected in org mode: this is
; required to come before (require 'evil) for some reason
(setq evil-want-C-i-jump nil)

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

; which major mode to load: default to text mode, use fortunate mode
; in files which end in the string 'fortunes', and always use LaTeX
; mode for .tex files: I don't write other TeX dialects
(setq default-major-mode 'text-mode)
(setq auto-mode-alist (cons '("\\fortunes$" . fortunate-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))

;; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-ignore-buffers '("\\` " "^\*"))
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
        "http://foodthroughthepages.com/feed/"
        "http://thepaternaldrunk.com/feed/"
        "http://pervocracy.blogspot.com/feeds/posts/default"
        "http://captainawkward.com/feed/"
        "http://elsiewrites.wordpress.com/feed/"
        "http://www.atomicnerds.com/?feed=rss2"
        "http://adventuresinolomouc.blogspot.com/feeds/posts/default?alt=rss"
        "http://feeds.feedburner.com/PagingDrNerdlove"))

;; mode hooks
; prog mode
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

; org mode
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines -1)))

; text mode
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

; tex mode
(add-hook 'tex-mode-hook 'refill-mode)
(add-hook 'tex-mode-hook (lambda () (set-input-method "british")))

; magit modes
(add-hook 'magit-mode-hook (lambda () (evil-local-mode -1)))
(add-hook 'magit-log-edit-mode-hook 'refill-mode)

; misc modes
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'calendar-mode-hook (lambda () (evil-local-mode -1)))
