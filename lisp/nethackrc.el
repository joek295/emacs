;;; Nethackrc mode

;; A mode for the nethackrc (and related) config files.
(require 'generic-x)

(define-generic-mode
    'nethackrc-mode
  '("#")
  '()
  '(
    ;; ("OPTIONS" . 'font-lock-builtin-face)
    ;; ("[A-Z]*COLOR" . 'font-lock-builtin-face)
    ("^[A-Z_]*" . 'font-lock-builtin-face)

    ("[a-z]*name" . 'font-lock-function-name-face)
    ("[a-z]*colors?" . 'font-lock-function-name-face)
    ("auto[a-z]*" . 'font-lock-function-name-face)
    ("show[a-z]*" . 'font-lock-function-name-face)
    ("monsters" . 'font-lock-function-name-face)
    ("boulder" . 'font-lock-function-name-face)
    ("cmdassist" . 'font-lock-function-name-face)
    ("time" . 'font-lock-function-name-face)
    ("msg_window" . 'font-lock-function-name-face)
    ("pickup[a-z_]*" . 'font-lock-function-name-face)
    ("[a-z_]*pet[a-z_]*" . 'font-lock-function-name-face)
    ("show[a-z]*" . 'font-lock-function-name-face)
    ("hilite[_a-z]*" . 'font-lock-function-name-face)

    (":.*" . 'font-lock-string-face)

    ("!" . 'font-lock-negation-char-face)
    )
  '("\\.nethackrc$" "\\.slashemrc$")
  nil
  "A mode for configuration files for nethack and its derivatives."
  )

(provide 'nh-config)
