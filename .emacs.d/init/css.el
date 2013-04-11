(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

(bundle less-css-mode
  (defvar less-css-output-file-name nil)
  (add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

  (defun less-css-compile-as (&optional file)
    (interactive)
    (let* ((minibuffer-local-completion-map
            (default-value 'minibuffer-local-completion-map))
           (file (or file less-css-output-file-name
                     (read-file-name "Compile to: " default-directory
                                     (and (fboundp 'less-css--output-path)
                                          (less-css--output-path))))))
      (setq less-css-output-file-name file)
      (less-css-compile)))
  (defun less-css-compile-with-timer ()
    (run-at-time "0 sec" nil #'less-css-compile-as))
  (add-hook 'less-css-mode-hook
            #'(lambda ()
                (add-hook 'after-save-hook #'less-css-compile-with-timer))))
