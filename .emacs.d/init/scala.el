(setq-default
 lsp-prefer-flymake nil
 )

(bundle scala-mode)
(bundle scala-bootstrap)
(bundle bloop)
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

(add-hook 'scala-mode-hook
          '(lambda ()
             (scala-bootstrap:with-metals-installed
              (scala-bootstrap:with-bloop-server-started
               (flycheck-mode)
               (auto-complete-mode -1)
               (require 'yasnippet)
               (require 'lsp-ui)
               (lsp)))))
