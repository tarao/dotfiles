(add-hook 'sh-mode-hook
          '(lambda ()
             (setq sh-basic-offset 4
                   sh-indentation 4
                   sh-indent-for-case-label 0
                   sh-indent-for-case-alt '+)))
