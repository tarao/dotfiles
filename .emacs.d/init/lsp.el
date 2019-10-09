(setq-default
 lsp-prefer-flymake nil
 lsp-ui-sideline-show-hover t
 lsp-ui-sideline-update-mode 'line
 )

(bundle lsp-mode)
(bundle lsp-ui
  (with-eval-after-load-feature 'lsp-ui
    (set-face-background 'lsp-ui-sideline-global "#444444")
    ))
(bundle flycheck)
(bundle yasnippet)
(bundle company-mode)
(bundle company-lsp)
(bundle helm-lsp)
