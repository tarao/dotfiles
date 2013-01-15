(bundle hatena-markup-mode)
(bundle hatena-multi-mode
  (setq-default hatena:mm:filetype-alist '((ocaml . tuareg))))
(bundle hatena-diary
  (setq-default hatena:d:major-mode 'hatena:markup-mode
                hatena:username "tarao")
  (eval-after-load-compile 'hatena-diary
    ;; multi-mode
    (add-hook 'hatena:markup-mode-hook #'hatena:multi-mode)))
