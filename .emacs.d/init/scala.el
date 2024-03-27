(bundle scala-mode)

(bundle lsp-mode)
(bundle lsp-ui)

(bundle lsp-metals)

(bundle flycheck)

(add-hook 'scala-mode-hook
          '(lambda ()
             (flycheck-mode)
             (auto-complete-mode -1)
             (require 'lsp-ui)
             (set (make-local-variable 'lsp-enable-indentation) nil)
             (lsp)))
