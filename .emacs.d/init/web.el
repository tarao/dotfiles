(eval-when-compile (require 'cl-lib))

(defconst web-mode-files
  '("\\.html$" "\\.tt$" "\\.mustache$" "\\.jsx?$" "\\.tsx?$"))
(bundle web-mode
  (setq-default web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2)
  (setq interpreter-mode-alist
        (cl-loop for (r . mode) in interpreter-mode-alist
                 collect (cons r (if (eq mode 'js-mode) 'web-mode mode))))
  (setq auto-mode-alist
        (append (mapcar '(lambda (x) (cons x 'web-mode)) web-mode-files)
                auto-mode-alist))
  (add-hook 'web-mode-hook '(lambda () (run-with-idle-timer 0 nil 'lsp))))
