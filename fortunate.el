;;; Fortunate mode

; based on the quip mode in the O'Reilly book "Writing GNU Emacs
; Extensions"

; currently the only difference from the quip mode in that book is the
; addition of the advice to save-buffer so that the fortunes file is
; recompiled on saving and the lack of a fortune mode key map.

(defvar fortunate-mode-hook nil
  "Functions to call when entering Fortunate Mode")

(define-derived-mode fortunate-mode text-mode "Fortunate"
  "Major mode for editing fortune files.

Special commands:
\\{fortunate-mode-map}"
  (kill-all-local-variables)
  (text-mode)
  (setq major-mode 'fortunate-mode)
  (setq mode-name "Fortunate")
  (use-loacl-map fortunate-mode-map)
  (run-hooks 'fortunate-mode-hook)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start "%\\I[ \t\n\^L]")
  (setq paragraph-separate "%$\\ [ \t\^L]*$")
  (make-local-variable 'page-delimiter)
  (setq page-delimiter "^%$")
  (defalias 'backward-fortune 'backward-page)
  (defalias 'forward-fortune 'forward-page)
  (defalias 'narrow-to-fortune 'narrow-to-page)
  (defalias 'what-fortune 'what-page) 
  )

(defun fortunate-save ()
  "Save and recompile file."
  (interactive)
  (save-buffer)
  (fortune-compile (buffer-file-name)))

(defun count-fortunes ()
  "Count the number of fortunes in the buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (prin1 (count-matches '"^%") t))))

(define-key fortunate-mode-map "\C-x\C-s" 'fortunate-save)
(define-key fortunate-mode-map "\C-xnf" 'narrow-to-fortune)
(define-key fortunate-mode-map "\C-x[" 'backward-fortune)
(define-key fortunate-mode-map "\C-x]" 'forward-fortune)
(define-key fortunate-mode-map "\C-c\C-w" 'what-fortune)

(provide 'fortunate)
