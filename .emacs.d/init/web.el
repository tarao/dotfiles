(defconst web-mode-files
  '("/templates/.*\\.html" "\\.mustache$"))
(bundle web-mode
  (setq-default web-mode-markup-indent-offset 2)
  (setq auto-mode-alist
        (append (mapcar #'(lambda (x) (cons x 'web-mode)) web-mode-files)
                auto-mode-alist)))