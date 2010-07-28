(setq TeX-default-mode 'japanese-latex-mode)
(setq auto-mode-alist
  (append
   '(("\\.sty$" . LaTeX-mode)
     ("\\.tex$" . LaTeX-mode))
   auto-mode-alist))
(setq japanese-TeX-command-default "pTeX")
(setq japanese-LaTeX-command-default "pLaTeX")
(setq latex-run-command "platex")
(setq default-file-coding-system-alist
      (append
       '(("\\.tex$" . euc-jp-unix)
         ("\\.sty$" . euc-jp-unix)
         ("\\.bib$" . euc-jp-unix))
       default-file-coding-system-alist))
(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
