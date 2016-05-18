(bundle hatena-markup-mode)
(with-eval-after-load-feature 'hatena-markup-mode
  (add-hook 'hatena:markup-mode-hook 'hatena:multi-mode)
  (add-hook 'hatena:markup-mode-hook
            #'(lambda ()
                (set (make-local-variable 'whitespace-style)
                     (remove 'lines-tail whitespace-style)))))
