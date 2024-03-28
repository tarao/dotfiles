(bundle scala-mode)

(bundle lsp-mode)
(bundle lsp-ui)

(bundle lsp-metals)

(bundle flycheck)

(add-to-list 'auto-mode-alist '("\\.sc$" . scala-mode))

(add-hook 'scala-mode-hook
          '(lambda ()
             (flycheck-mode)
             (auto-complete-mode -1)
             (require 'lsp-ui)
             (set (make-local-variable 'lsp-enable-indentation) nil)
             (lsp)))
