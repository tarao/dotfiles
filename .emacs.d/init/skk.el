(setq-default skk-init-file (expand-file-name "init/.skk" user-emacs-directory)
              skk-user-directory "~/.ddskk")
(bundle ddskk
  (global-set-key (kbd "C-x C-j") 'skk-mode))

;; prevent dired-x from binding C-x C-j
(setq-default dired-bind-jump nil)

(setq mode-line-format
      (append '("" skk-modeline-input-mode) mode-line-format))
