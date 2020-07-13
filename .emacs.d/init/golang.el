(bundle flycheck)
(bundle go-mode)

(bundle lsp-mode)
(bundle lsp-ui)

(defmacro with-gopls-installed (&rest body)
  `(if (executable-find "gopls")
       (progn
         ,@body)
     (let ((buf (current-buffer))
           (proc-buf (get-buffer-create "*install-gopls*"))
           (body ',body))
       (make-process
        :name "install-gopls"
        :buffer proc-buf
        :command (list "go" "get" "golang.org/x/tools/gopls")
        :sentinel `(lambda (proc event)
                     (when (and (eq (process-status proc) 'exit)
                                (= 0 (process-exit-status proc)))
                       (with-current-buffer ,buf
                         ,@body)))))))

(add-hook 'go-mode-hook '(lambda ()
                           (with-gopls-installed
                            (add-hook 'before-save-hook 'lsp-format-buffer t t)
                            (add-hook 'before-save-hook 'lsp-organize-imports t t)
                            (lsp))))
