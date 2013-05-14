(bundle markdown-mode
  (add-hook 'markdown-mode-hook
            #'(lambda ()
                (set (make-local-variable 'whitespace-style)
                     (remove 'lines-tail whitespace-style)))))
