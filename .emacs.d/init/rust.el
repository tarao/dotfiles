(bundle rust-mode)
(bundle cargo)
(bundle lsp-mode)
(bundle lsp-ui)

(setq-default lsp-rust-analyzer-cargo-watch-command "clippy")

(add-hook 'rust-mode-hook
          '(lambda ()
             (cargo-minor-mode)
             (lsp)))
