(bundle graphviz-dot-mode
  (setq-default graphviz-dot-indent-width 2)
  (defun graphviz-dot/preview-after-compilation (buffer desc)
    (eval-and-compile (require 'graphviz-dot-mode))
    (when (string-prefix-p "finished" desc)
      (with-current-buffer compilation-original-buffer
        (graphviz-dot-preview))))
  (add-hook 'graphviz-dot-mode-hook
            #'(lambda ()
                (add-hook 'compilation-finish-functions
                          #'graphviz-dot/preview-after-compilation))))
