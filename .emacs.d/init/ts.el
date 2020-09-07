(setq-default typescript-indent-level 2)

(defconst ts-mode-files '("\\.ts$"))

;; Don't treat `#!/usr/bin/env node` as JS; TS file may have this shebang.
;; See https://github.com/Microsoft/TypeScript/issues/2749
(setq interpreter-mode-alist (assoc-delete-all "node" interpreter-mode-alist))

(bundle typescript-mode
  (setq auto-mode-alist
        (append (mapcar #'(lambda (x) (cons x 'typescript-mode)) ts-mode-files)
                auto-mode-alist))
  (add-hook 'typescript-mode-hook 'lsp))
