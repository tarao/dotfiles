(setq-default sh-basic-offset 4
              sh-indentation 4
              sh-indent-for-case-label 0
              sh-indent-for-case-alt '+)
(setq auto-mode-alist
      (append
       '(("\\.sh$" . sh-mode)
         ("\\.csh$" . sh-mode)
         ("\\.tcsh$" . sh-mode)
         ("\\.bash$" . sh-mode)
         ("\\.zsh$" . sh-mode))
       auto-mode-alist))
