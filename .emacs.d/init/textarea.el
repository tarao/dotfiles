(bundle textarea
  (add-hook 'textarea:local-mode-hook #'textarea:start-server))
(eval-after-load-compile 'end-mark
  (add-to-list 'end-mark-mode-buffers-regexp "^\\*textarea\\*$"))
