(bundle cvim-edit
  (cvim-edit:server-start)
  (with-eval-after-load-feature 'cvim-edit
    (setq cvim-edit:major-mode 'markdown-mode))
  (add-hook 'cvim-edit:local-mode-hook '(lambda () (end-mark-mode))))
