(bundle textarea)
(eval-after-load-compile 'end-mark
  (add-to-list 'end-mark-mode-buffers-regexp "^\\*textarea\\*$"))
