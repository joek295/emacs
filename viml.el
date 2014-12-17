;; VimL mode

; A mode for VimScript. Because there doesn't appear to be one, and I
; want syntax highlighting when I look at my .vimrc file in Emacs.

(define-generic-mode
    'viml-mode
  '("\"")
  '("set" "map" "noremap" "vmap" "imap" "nmap" "inoremap" "nnoremap" "vnoremap" "let" "colorscheme" "filetype" "autocmd" "syntax")
  '(("=\\|return" . 'font-lock-operator)
    ("on" . 'font-lock-builtin))
  '("\\.vimrc$")
  nil
  "A mode for VimL files")

(provide 'viml)
