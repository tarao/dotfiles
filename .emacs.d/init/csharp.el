(bundle csharp-mode
  (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode)))

;; use normal brace
(defadvice csharp-insert-open-brace
  (around ad-suppress-csharp-insert-open-brace activate)
  (call-interactively 'c-electric-brace))
