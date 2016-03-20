;;; brainfuck mode

;; A mode for the brainfuck programming language.
(require 'generic-x)

(define-generic-mode
    'brainfuck-mode
  '()
  '()
  '(
    (">" . 'font-lock-function-name-face)
    ("<" . 'font-lock-function-name-face)
    ("," . 'font-lock-warning-face)
    ("\\." . 'font-lock-warning-face)
    ("-" . 'font-lock-keyword-face)
    ("+" . 'font-lock-keyword-face)
    ("[^][.+,<>-]" . 'font-lock-comment-face)
    )
  '("\\.bf$")
  nil
  "Syntax highlighting mode for the brainfuck programming language."
  )

(provide 'brainfuck)
