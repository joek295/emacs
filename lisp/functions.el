;;; Functions

; Miscellaneous functions written in elisp.  Too short and simple to
; require a separate file, but often very useful.  Mostly written by
; myself, but some taken from various places on the web.  This file
; also contains advice for existing Emacs functions.

;; Buffer manipulation
(defun switch-to-previous-buffer ()
  "Switch the open buffer to the previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun quickswitch ()
  "Switch between a given buffer (by default this is *scratch*)
and the currently open buffer.

If the variable 'quickbuffer' is defined, switch to that buffer
if in any other buffer, or to the previous buffer if in that
buffer. If not, set it to *scratch*, and then switch."
  (interactive)
  (if (boundp 'quickbuffer)
      (if (equal (buffer-name) quickbuffer)
          (switch-to-buffer (other-buffer 1))
        (switch-to-buffer quickbuffer))
    (setq quickbuffer "*scratch*")
    (switch-to-buffer (other-buffer 1))))

(defun scratch-scratch ()
  "Switch to the scratch buffer. Clear the scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*")
  (kill-this-buffer))

(defadvice switch-to-buffer (before save-buffer-now activate)
  "Save buffer before switching."
  (when buffer-file-name (save-buffer)))

(defun kill-active-buffer ()
  "Kill the active buffer.

Essentially the same as M-x kill-buffer <RET>."
  (interactive)
  (kill-buffer (buffer-name)))

;; Window splitting
(defun split-window-right-and-switch ()
  "Splits the window vertically; switches to the newly split
window; and opens the next buffer rather than the current one in
that window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (next-buffer))

(defadvice other-window (before other-window-now activate)
  "Save buffer before changing focused window.

When a buffer points to an existing file (rather than being a
non-user buffer), then save the buffer whenever switching away
from it to another window."
  (when buffer-file-name (save-buffer)))

(defun swap-windows ()
  "If you have 2 windows, swap them.

Function taken from Steve Yegge's blog."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let*
             ((w1 (first (window-list)))
              (w2 (second (window-list)))
              (b1 (window-buffer w1))
              (b2 (window-buffer w2))
              (s1 (window-start w1))
              (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

;; Navigating withing a file
(defun smart-move-to-beginning-of-line ()
  "If the point is at the first non-whitespace character in the
line, move to the beginning of the line.  Otherwise, move to the
first non-whitespace character.

Note that this works even if the point is before the first
non-whitespace character.  Thus this function can be used to
cycle between the line start and the first non-whitespace
character.

This is based off (directly copied from?) something I found
online, though I can't remember where."
  (interactive)
; remember where point originally was and run back-to-indentation
  (let ((orig-point (point)))
    (back-to-indentation)
; if point has not moved, then run move-to-beginning-of-line
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; File Manipulation

; taken from Steve Yegge's blog.
(defun rename-file-and-buffer (new-name)
  "Rename both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))) 

; taken from Steve Yegge's blog.
(defun move-file-and-buffer (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\(?:/\|\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil) t))))

; Taken from What the Emacs.d?!
(defun delete-file-and-buffer ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; Coding

; Taken from emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere"
(defun eval-and-replace ()
  "Replace the preceding sexp with its value.

If the sexp throws an error, do not replace the
expression. Simply give an error message."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun indent-buffer ()
  "Re-indent the entire buffer.

Delete all trailing whitespace, re-indent, and then untabify the
entire buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  )

;; Misc
(defun freqanalyse ()
  "Frequency Analyse the open buffer.

Currently works by using the `fran` utility at ~/scripts/fran.

In the future, will be implemented in elisp (allowing it to work
on non-user buffers e.g. *scratch*).

Should also implement something telling us the percentage of the
total text each character is used."
  (interactive)
  (shell-command (format "%s%s" "fran " (buffer-file-name))))

(defun my-toggle-input-method ()
  "Toggle between TeX and Greek inputs.

When input-method is TeX, set it to Greek; otherwise set it to
TeX.  Remember that TeX can most of the time be used in exactly
the same way as British; only when writing LaTeX escapes, for
example when editing a LaTeX document, do you have to remember to
run toggle-input-method (C-\)."
  (interactive)
  (if (equal current-input-method "TeX")
        (set-input-method "greek")
    (set-input-method "TeX")))

(defun newline-below ()
  "create an empty line below the point."
  (interactive)
  (save-excursion
    (end-of-line)
    (newline)))

(defun newline-above ()
  "create an empty line above the point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (newline)))

;(defun latex-emphasise-word ()
;  "surround the word under the point with \emph{}."
;  )

(defun my-dired-up-directory ()
  "Dired up directory and kill old dired buffer.
From Nathan Typanski's blogpost 'Towards a vim-like emacs config'."
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)
    ))
