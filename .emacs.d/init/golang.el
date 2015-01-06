(bundle flycheck)
(bundle go-autocomplete)
(bundle go-eldoc)
(bundle go-mode
  (with-eval-after-load-feature 'go-mode
    (require 'go-autocomplete)
    (setq gofmt-command "goimports")
    (add-hook 'go-mode-hook #'go-eldoc-setup)
    (add-hook 'go-mode-hook #'flycheck-mode)
    (add-hook 'before-save-hook #'gofmt-before-save)))
