;;; Shortlines mode

(defvar shortlines-mode-hook nil
  "Functions to call when entering Shortlines Mode")

(define-derived-mode shortlines-mode text-mode "Shortlines"
  "Major mode for editing text files which require hard-line
wrapping. Re-fill these files on-the-fly.

Examples of such files include:

* NEWS
* README"
  (kill-all-local-variables)
  (text-mode)
  (setq major-mode 'shortlines-mode)
  (setq mode-name "Shortlines")
  (run-hooks 'shortlines-mode-hook)
  (refill-mode)
  )

(provide 'shortlines)
