(setq-default
 sbt:ansi-support t
 lsp-prefer-flymake nil
 )

(bundle scala-mode)
(bundle scala-bootstrap)
(bundle sbt-mode
  (defun scala/sbt-do-test ()
    (interactive)
    (sbt-command "test"))

  (defun scala/sbt-do-compile ()
    (interactive)
    (sbt-command "test:compile"))

  (defun scala/sbt-do-clean ()
    (interactive)
    (sbt-command "clean"))

  (defun scala/sbt-do-console ()
    (interactive)
    (sbt-command "consoleQuick")))
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
