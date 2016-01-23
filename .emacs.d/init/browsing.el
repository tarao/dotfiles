(when (executable-find "xdg-open")
  (with-eval-after-load-feature 'browse-url
    (setq browse-url-generic-program "xdg-open"
          browse-url-browser-function 'browse-url-generic)))
