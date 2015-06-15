;;; elfeed mode configuration:
(use-package elfeed
  :ensure elfeed
  :config
  (progn
    (setq elfeed-feeds
          '("http://www.terminally-incoherent.com/blog/feed/"
            "http://kmandla.wordpress.com/feed/"
            "http://debianjoe.wordpress.com/feed/"
            "http://inconsolation.wordpress.com/feed/"
            "http://foodthroughthepages.wordpress.com/feed/"
            "http://thepaternaldrunk.com/feed/"
            "http://pervocracy.blogspot.com/feeds/posts/default"
            "http://captainawkward.com/feed/"
            "http://elsiewrites.wordpress.com/feed/"
            "http://www.atomicnerds.com/?feed=rss2"
            "http://adventuresinolomouc.blogspot.com/feeds/posts/default?alt=rss"
            "http://samreadsatlasshrugged.wordpress.com/feed/"
            "http://sexgeek.wordpress.com/feed"
            "http://feeds.feedburner.com/PagingDrNerdlove"))

    (defun my-elfeed-search-keys ()
      "Modify the default elfeed keymap"
      (local-set-key "RET" 'elfeed-browse-url))

    (defun my-elfeed-mode-hook ()
      (my-elfeed-search-keys)
      )
    (add-hook 'elfeed-mode-hook 'my-elfeed-mode-hook)
    )
  )
