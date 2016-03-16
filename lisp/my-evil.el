(defun my-intelligent-compile ()
  "Intelligently compile things.  Use M-x compile for .c files,
  compile .tex files, and export .org files to HTML."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-html-export-to-html)
    (compile)
    )
  )

(use-package evil-leader
  :ensure evil-leader
  :demand evil-leader
  :init
  (global-evil-leader-mode t)
  :config (progn
            (defun my-evil-upcase-WORD ()
              "same as <esc>gUaW"
              (interactive)
              (save-excursion
                (evil-backward-WORD-begin) 
                (evil-upcase (point) (progn (evil-forward-WORD-end) (point)))
                )
              )

            (evil-leader/set-leader "<SPC>")
            (evil-leader/set-key
              "c" 'my-intelligent-compile
              "f" 'fill-paragraph
              "n" 'forward-paragraph
              "p" 'backward-paragraph
              "j" 'open-line
              "o" 'newline-below
              "O" 'newline-above
              "u" 'my-evil-upcase-WORD
              ;; Modes:
              "d" 'dired
              "b" 'ibuffer
              "g" 'magit-status
              "#" 'comment-or-uncomment-region
              )
            )
  )

(use-package evil
  :ensure evil
  :demand evil
  :config (progn
            (evil-mode 1)
            ;; Evil-mode advice
            ;; Both advice for evil functions, and advice which only
            ;; needs to take effect when evil mode is active goes in
            ;; this section
            (defadvice evil-goto-line (after evil-goto-line-and-center activate)
              "When using the goto line command ('G'), try to center the line"
              (evil-scroll-line-to-center (line-number-at-pos)))
            (defadvice evil-ex-search-next (after evil-find-key-and-center activate)
              "When using the search next command ('n'), try to center the line"
              (evil-scroll-line-to-center (line-number-at-pos)))
            (defadvice switch-to-buffer (before return-to-normal-now activate)
              "In evil mode, return to normal state before switching buffer"
              (when evil-mode
                (evil-normal-state)))
            (defadvice other-window (before other-window-normal-now activate)
              "In evil mode, return to normal state before switching window"
              (when evil-mode
                (evil-normal-state)))

            ;; Evil-mode functions
            (defun evil-yank-to-eol ()
              "Yank to end-of-line in evil mode.  Useful for making the
  behaviour of Y sane, i.e. analagous to D, C etc., and not the
  same as yy."
              (interactive)
              (evil-yank (point) (point-at-eol)))

            (defun my-evil-join-above ()
              "Join a line with the line above it."
              (interactive)
              (save-excursion
                (join-line)
                )
              )

            ;; Evil-mode keybindings
            ;; Define evil keys for normal state:
            (progn
              (define-key evil-normal-state-map "L" 'evil-end-of-line)
              (define-key evil-normal-state-map "H" 'smart-move-to-beginning-of-line)
              (define-key evil-normal-state-map "K" 'my-evil-join-above)
              (define-key evil-normal-state-map "Y" 'evil-yank-to-eol)
              (define-key evil-normal-state-map "'" 'evil-goto-mark)
              (define-key evil-normal-state-map "`" 'evil-goto-mark-line)
              (define-key evil-normal-state-map "zg" 'my-save-word)
              (define-key evil-normal-state-map "]s" 'flyspell-goto-next-error)
              (define-key evil-normal-state-map (kbd "C-<right>") 'paredit-forward-slurp-sexp)
              (define-key evil-normal-state-map (kbd "C-<left>") 'paredit-forward-barf-sexp)
              (define-key evil-normal-state-map (kbd "C-S-<right>") 'paredit-backward-slurp-sexp)
              (define-key evil-normal-state-map (kbd "C-S-<left>") 'paredit-backward-barf-sexp)
              (define-key evil-normal-state-map (kbd "C-a") 'my-increment-integer-at-point)
              )

            ;; Define evil keys in insert state:
            (progn
              (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
              (define-key evil-insert-state-map (kbd "C-<right>") 'paredit-forward-slurp-sexp)
              (define-key evil-insert-state-map (kbd "C-<left>") 'paredit-forward-barf-sexp)
              (define-key evil-normal-state-map (kbd "C-S-<right>") 'paredit-backward-slurp-sexp)
              (define-key evil-normal-state-map (kbd "C-S-<left>") 'paredit-backward-barf-sexp)
              )


            ;; Define evil keys in visual state:
            (progn
              (define-key evil-visual-state-map (kbd "C-g") 'evil-normal-state)
              (define-key evil-visual-state-map (kbd "C-<right>") 'paredit-forward-slurp-sexp)
              (define-key evil-visual-state-map (kbd "C-<left>") 'paredit-forward-barf-sexp)
              (define-key evil-normal-state-map (kbd "C-S-<right>") 'paredit-backward-slurp-sexp)
              (define-key evil-normal-state-map (kbd "C-S-<left>") 'paredit-backward-barf-sexp)
              )

            ;; Define evil keys in replace state:
            (progn
              (define-key evil-replace-state-map (kbd "C-g") 'evil-normal-state))

            (progn
              (define-key evil-operator-state-map "L" 'evil-end-of-line)
              (define-key evil-operator-state-map "H" 'smart-move-to-beginning-of-line)
              )
            ;; Define evil keys in normal state when prog-mode is active
            (evil-define-key 'normal prog-mode-map
              (kbd "C-c i") 'indent-buffer
              )

            ;; Define evil keys in visual state when prog-mode is active
            (evil-define-key 'visual prog-mode-map
              (kbd "C-c i") 'indent-region
              )

            ;; Define evil keys in normal state when org-mode is active
            (evil-define-key 'normal org-mode-map
              (kbd "TAB") 'org-cycle
              (kbd "M-h") 'org-metaleft
              (kbd "M-j") 'org-metadown
              (kbd "M-k") 'org-metaup
              (kbd "M-l") 'org-metaright
              )

            ;; Define evil keys in normal state when eww-mode is active
            (evil-define-key 'normal eww-mode-map
              "q" 'kill-active-buffer
              (kbd "DEL") 'eww-back-url
              )

            ;; Define evil keys in normal state when dired-mode is active
            (evil-define-key 'normal dired-mode-map
              "l" 'dired-find-alternate-file
              (kbd "RET") 'dired-find-alternate-file
              "h" 'my-dired-up-directory
              "q" 'kill-this-buffer
              "n" 'evil-search-next
              "N" 'evil-search-previous
              )

            ;; Define evil keys in normal state when ibuffer-mode is active
            (evil-define-key 'normal ibuffer-mode-map
              "j" 'ibuffer-forward-line
              "k" 'ibuffer-backward-line
              "l" 'ibuffer-visit-buffer
              "U" 'ibuffer-unmark-backward
              "/" 'ibuffer-jump-to-buffer
              )

            ;; Define evil keys in emacs state when ibuffer-mode is active
            (evil-define-key 'emacs ibuffer-mode-map
              "j" 'ibuffer-forward-line
              "k" 'ibuffer-backward-line
              "l" 'ibuffer-visit-buffer
              "U" 'ibuffer-unmark-backward
              "/" 'ibuffer-jump-to-buffer
              )

            (evil-define-key 'normal calendar-mode-map
              "l" 'calendar-forward-day
              "h" 'calendar-backward-day
              "j" 'calendar-forward-week
              "k" 'calendar-backward-week
              ")" 'calendar-forward-month
              "(" 'calendar-backward-month
              "}" 'calendar-forward-year
              "{" 'calendar-backward-year
              "." 'calendar-goto-today
              "<" 'calendar-scroll-right-three-months
              ">" 'calendar-scroll-left-three-months
              "q" 'calendar-exit
              )
            )
  )
