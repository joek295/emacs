;;; Fortunate mode

; based on the quip mode in the O'Reilly book "Writing GNU Emacs
; Extensions"

; currently the only difference from the quip mode in that book is the
; addition of the advice to save-buffer so that the fortunes file is
; recompiled on saving and the lack of a fortune mode key map.

(defvar fortunate-mode-hook nil
  "Functions to call when entering Fortunate Mode")

(define-derived-mode fortunate-mode text-mode "Fortunate"
  "Major mode for editing fortune files."
  (kill-all-local-variables)
  (text-mode)
  (setq major-mode 'fortunate-mode)
  (setq mode-name "Fortunate")
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
  (defadvice save-buffer (after fortunes-recompile-now activate)
    "Recompile fortunes file after saving."
    (fortune-compile (buffer-file-name)))
  )


(defun count-fortunes ()
  "Count the number of fortunes in the buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (prin1 (count-matches '"^%") t))))

(provide 'fortunate)
