(bundle lsp-pyright
  (add-hook 'python-mode-hook
            '(lambda ()
               (require 'lsp-pyright)
               (lsp))))
