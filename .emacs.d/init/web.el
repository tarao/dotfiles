(defconst web-mode-files
  '("\\.html$" "\\.tt$" "\\.mustache$" "\\.tsx$"))
(bundle web-mode
  (setq-default web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2)
  (setq auto-mode-alist
        (append (mapcar #'(lambda (x) (cons x 'web-mode)) web-mode-files)
                auto-mode-alist)))
