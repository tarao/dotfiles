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
(bundle lsp-docker)
(bundle lsp-ui
  (with-eval-after-load-feature 'lsp-ui
    (set-face-background 'lsp-ui-sideline-global "#444444")
    )
  (with-eval-after-load-feature 'lsp-ui-imenu
    (defun adjust-lsp-ui-imenu-window (orig-fun &rest args)
      (eval-and-compile (require 'lsp-ui-imenu))
      (save-window-excursion
        (let ((inhibit-frame-expansion t))
          (apply orig-fun args)))
      (let ((buf (get-buffer "*lsp-ui-imenu*")))
        (display-buffer-in-side-window buf `((side . left)
                                             (window-size . ,lsp-ui-imenu-window-width)))
        (let ((win (get-buffer-window buf)))
          (select-window win)
          (set-window-dedicated-p win t)
          (window-resize win (- lsp-ui-imenu-window-width (window-width win)) t))))
    (advice-add 'lsp-ui-imenu :around 'adjust-lsp-ui-imenu-window))
  (add-hook 'lsp-mode-hook '(lambda () (require 'lsp-ui))))
(bundle lsp-treemacs)
(bundle dap-mode
  (add-hook 'lsp-mode-hook #'dap-mode)
  (add-hook 'lsp-mode-hook #'dap-ui-mode)
  (with-eval-after-load-feature 'dap-ui
    (setcdr (assoc "*dap-ui-breakpoints*" dap-ui-buffer-configurations)
            `((side . left) (slot . 2) (window-width . ,sidebar-width)))
    (setcdr (assoc "*dap-ui-sessions*" dap-ui-buffer-configurations)
            `((side . left) (slot . 3) (window-width . ,sidebar-width)))
    (setcdr (assoc "*dap-ui-locals*" dap-ui-buffer-configurations)
            `((side . left) (slot . 4) (window-width . ,sidebar-width)))
    (setcdr (assoc "*dap-ui-expressions*" dap-ui-buffer-configurations)
            `((side . bottom) (slot . 4) (window-width . ,(+ 2 sidebar-width))))
    ))
(bundle posframe)
(bundle flycheck)
(bundle yasnippet
  (add-hook 'lsp-mode-hook #'yas-minor-mode))
(bundle company-mode
  (add-hook 'lsp-mode-hook
            '(lambda ()
               (when (and (boundp 'auto-complete-mode) auto-complete-mode)
                 (auto-complete-mode -1))
               (company-mode))))
(bundle helm-lsp)

(defun adjust-lsp-treemacs-symbols-window ()
  (let* ((buf (get-buffer "*LSP Symbols List*"))
         (win (get-buffer-window buf))
         (width (window-width win))
         (window-size-fixed))
    (window-resize win (- sidebar-width width) t)))
(advice-add 'lsp-treemacs-symbols :after #'adjust-lsp-treemacs-symbols-window)
