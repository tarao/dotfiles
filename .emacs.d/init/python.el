(bundle lsp-pyright
  (add-hook 'python-mode-hook
            '(lambda ()
               (auto-complete-mode -1)
               (require 'lsp-pyright)
               (lsp))))
