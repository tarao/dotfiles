(setq-default
 lsp-prefer-flymake nil
 lsp-ui-sideline-show-hover t
 lsp-ui-sideline-update-mode 'line
 lsp-ui-imenu-window-width sidebar-width
 lsp-ui-imenu-colors '("#7F9F7F" "#F0DFAF" "#8CD0D3" "#93E0E3" "#DFAF8F")
 lsp-file-watch-threshold nil
 )

(bundle lsp-mode
  (add-hook 'lsp-mode-hook 'lsp-lens-mode))
(bundle lsp-ui
  (with-eval-after-load-feature 'lsp-ui
    (set-face-background 'lsp-ui-sideline-global "#444444")
    ))
(bundle dap-mode
  (add-hook 'lsp-mode-hook #'dap-mode)
  (add-hook 'lsp-mode-hook #'dap-ui-mode))
(bundle posframe)
(bundle flycheck)
(bundle yasnippet
  (add-hook 'lsp-mode-hook #'yas-minor-mode))
(bundle company-mode
  (add-hook 'lsp-mode-hook #'company-mode))
(bundle helm-lsp)

(defun adjust-lsp-ui-imenu-window (orig-fun &rest args)
  (save-window-excursion
    (let ((inhibit-frame-expansion t))
      (apply orig-fun args)))
  (let ((buf (get-buffer "*lsp-ui-imenu*")))
    (display-buffer-in-side-window buf `((side . left)
                                         (window-size . ,lsp-ui-imenu-window-width)))
    (let ((win (get-buffer-window buf)))
      (select-window win)
      (lsp-ui-imenu--move-to-name-beginning)
      (set-window-dedicated-p win t)
      (window-resize win (- lsp-ui-imenu-window-width (window-width win)) t))))
(advice-add 'lsp-ui-imenu :around #'adjust-lsp-ui-imenu-window)
