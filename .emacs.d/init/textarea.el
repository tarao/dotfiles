(bundle textarea
  (add-hook 'textarea:local-mode-hook 'textarea:start-server))
(with-eval-after-load-feature 'end-mark
  (add-to-list 'end-mark-mode-buffers-regexp "^\\*textarea\\*$"))
