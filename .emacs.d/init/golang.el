(bundle flycheck)
(bundle go-mode)

(bundle lsp-mode)
(bundle lsp-ui)

(add-hook 'go-mode-hook
          '(lambda ()
             (unless (executable-find "gopls")
               (call-process-shell-command "go get golang.org/x/tools/gopls@latest"))
             (add-hook 'before-save-hook 'lsp-format-buffer t t)
             (add-hook 'before-save-hook 'lsp-organize-imports t t)
             (lsp)))
