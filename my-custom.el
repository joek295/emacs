(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote eww-browse-url))
 '(evil-overriding-maps
   (quote
    ((Buffer-menu-mode-map)
     (color-theme-mode-map)
     (comint-mode-map)
     (compilation-mode-map)
     (dictionary-mode-map)
     (ert-results-mode-map . motion)
     (Info-mode-map . motion)
     (speedbar-key-map)
     (speedbar-file-key-map)
     (speedbar-buffers-key-map)
     (elfeed-search-mode-map)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "black"))))
 '(linum ((t (:inherit (shadow default) :background "black"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "brightred" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :weight bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green" :weight bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue" :weight bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "color-34" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "color-16" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "white" :weight bold))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "color-81" :weight bold))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "magenta" :weight extra-bold)))))
