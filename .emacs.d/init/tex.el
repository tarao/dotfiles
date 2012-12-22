(setq TeX-default-mode 'japanese-latex-mode
      auto-mode-alist
      (append
       `(("\\.sty$" . ,TeX-default-mode)
         ("\\.tex$" . ,TeX-default-mode))
       auto-mode-alist)
      japanese-TeX-command-default "pTeX"
      japanese-LaTeX-command-default "pLaTeX"
      default-file-coding-system-alist
      (append
       '(("\\.tex$" . euc-jp-unix)
         ("\\.sty$" . euc-jp-unix)
         ("\\.bib$" . euc-jp-unix))
       default-file-coding-system-alist))
(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
