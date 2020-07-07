(bundle scala-mode)
(bundle scala-bootstrap)
(bundle bloop)

(bundle lsp-mode)
(bundle lsp-ui)
(bundle lsp-treemacs)

(bundle lsp-metals)

(bundle flycheck)

(add-hook 'scala-mode-hook
          '(lambda ()
             (scala-bootstrap:with-metals-installed
              (scala-bootstrap:with-bloop-server-started
               (flycheck-mode)
               (auto-complete-mode -1)
               (require 'lsp-ui)
               (lsp)))))
