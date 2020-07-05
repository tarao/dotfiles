(setq-default
 lsp-prefer-flymake nil
 lsp-ui-sideline-show-hover t
 lsp-ui-sideline-update-mode 'line
 lsp-file-watch-threshold nil
 )

(bundle lsp-mode
  (add-hook 'lsp-mode-hook 'lsp-lens-mode))
(bundle lsp-ui
  (with-eval-after-load-feature 'lsp-ui
    (set-face-background 'lsp-ui-sideline-global "#444444")
    ))
(bundle dap-mode
  (add-hook 'lsp-mode #'dap-mode)
  (add-hook 'lsp-mode #'dap-ui-mode))
(bundle posframe)
(bundle flycheck)
(bundle yasnippet)
(bundle company-mode)
(bundle company-lsp)
(bundle helm-lsp)
