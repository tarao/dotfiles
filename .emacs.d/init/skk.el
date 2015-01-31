(setq-default skk-init-file "init/.skk"
              skk-user-directory "~/.ddskk")
(bundle ddskk
  (global-set-key (kbd "C-x C-j") 'skk-mode))

;; prevent dired-x from binding C-x C-j
(setq-default dired-bind-jump nil)

(setq mode-line-format
      (append '("" skk-modeline-input-mode) mode-line-format))
