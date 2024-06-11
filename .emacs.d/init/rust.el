(bundle rust-mode)
(bundle cargo)
(bundle lsp-mode)
(bundle lsp-ui)

(setq-default lsp-rust-analyzer-cargo-watch-command "clippy")

(add-hook 'rust-mode-hook
          '(lambda ()
             (auto-complete-mode -1)
             (cargo-minor-mode)
             (require 'lsp-ui)
             (lsp)))
