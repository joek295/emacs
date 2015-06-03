;;; BindKeys mode

;; A mode for xbindkeys.
(require 'generic-x)

(define-generic-mode
    'xbindkeys-mode
  '("#")
  '("Mod1" "Mod2" "Mod3" "Mod4" "Mod5" "Shift" "Control" "Release"
    "mod1" "mod2" "mod3" "mod4" "mod5" "shift" "control" "release")
  '(
    ("[a-zA-Z]" . 'font-lock-function-name-face)
    ("space" . 'font-lock-function-name-face)
    ("period" . 'font-lock-function-name-face)
    ("comma" . 'font-lock-function-name-face)
    ("Return" . 'font-lock-function-name-face)
    )
  '("\\.xbindkeysrc$")
  nil
  "A mode for the .xbindkeysrc file."
  )

(provide 'xbindkeys)
