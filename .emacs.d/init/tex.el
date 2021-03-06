(when (executable-find "platex")
  (bundle auctex
    (defconst TeX-default-mode 'japanese-latex-mode)
    (defconst TeX-mode-files '("\\.sty$" "\\.tex$"))

    (dolist (ext TeX-mode-files)
      (add-to-list 'auto-mode-alist (cons ext TeX-default-mode)))

    (setq-default japanese-TeX-command-default "pTeX"
                  japanese-LaTeX-command-default "pLaTeX")

    (add-hook 'TeX-mode-hook #'turn-on-reftex)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)))
