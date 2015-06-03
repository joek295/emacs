;;; VimL mode

;; A mode for VimScript. Because there doesn't appear to be one, and I
;; want syntax highlighting when I look at my .vimrc file in Emacs.
(require 'generic-x)

(define-generic-mode
    'viml-mode
  '("\"")
  '("set" "let" "colorscheme" "filetype" "autocmd" "syntax" "au"
    "map" "cmap" "imap" "nmap" "vmap"
    "noremap" "cnoremap" "inoremap" "nnoremap" "vnoremap")
  '(
    ("formatoptions" . 'font-lock-preprocessor-face)
    ("incsearch" . 'font-lock-preprocessor-face)
    ("laststatus" . 'font-lock-preprocessor-face)
    ("linebreak" . 'font-lock-preprocessor-face)
    ("no[a-z]*backup" . 'font-lock-preprocessor-face)
    ("nocompatible" . 'font-lock-preprocessor-face)
    ("noswapfile" . 'font-lock-preprocessor-face)
    ("number" . 'font-lock-preprocessor-face)
    ("shortmess" . 'font-lock-preprocessor-face)
    ("showmatch" . 'font-lock-preprocessor-face)
    ("spell[a-z]*" . 'font-lock-preprocessor-face)
    ("statusline" . 'font-lock-preprocessor-face)

    ("[a-z ]:[A-Za-z_]+" . 'font-lock-function-name-face)

    ("enable" . 'font-lock-constant-face)
    ("Buf[A-Z][A-Za-z]*" . 'font-lock-constant-face)
    ("FocusLost" . 'font-lock-constant-face)
    ("FileType" . 'font-lock-constant-face)
    ("\\bon\\b" . 'font-lock-constant-face) 
    ("\\boff\\b" . 'font-lock-constant-face)

    ("\\(end\\)?if" . 'font-lock-builtin-face)
    ("\\(end\\)?func\\(tion\\)?" . 'font-lock-builtin-face)

    ("<.*?>" . 'font-lock-string-face)
    ("'.*?'" . 'font-lock-string-face)

    ("getline" . 'font-lock-type-face)
    ("substitute" . 'font-lock-type-face)

    ("=" . 'font-lock-negation-char-face)
    ("+" . 'font-lock-negation-char-face)
    ("-" . 'font-lock-negation-char-face)
    )
  '("\\.vimrc$")
  nil
  "A mode for VimL files.  This is mainly used by the .vimrc file."
  )

(provide 'viml)
