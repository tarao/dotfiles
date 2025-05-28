(bundle emacswiki:visual-basic-mode
  (setq auto-mode-alist (append '(("\\.vbs" . visual-basic-mode)
                                  ("\\.asp" . visual-basic-mode))
                                auto-mode-alist))
  (setq-default visual-basic-mode-indent 4)
  (add-hook 'visual-basic-mode-hook
            #'(lambda ()
                (auto-complete-mode 1)
                (undo-tree-mode 1)
                (whitespace-mode 1)
                (outline-indent-minor-mode 1)
               )))
