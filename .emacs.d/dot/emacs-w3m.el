(setq w3m-command-arguments
      (nconc w3m-command-arguments
             '("-o" "http_proxy=http://localhost:3128/")))
(setq w3m-home-page "http://www.google.com/")
(setq w3m-use-cookies t)
