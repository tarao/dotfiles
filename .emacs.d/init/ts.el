(defconst ts-mode-files '("\\.ts$"))

(bundle typescript-mode
  (setq auto-mode-alist
        (append (mapcar #'(lambda (x) (cons x 'typescript-mode)) ts-mode-files)
                auto-mode-alist))
  (add-hook 'typescript-mode-hook 'lsp))
