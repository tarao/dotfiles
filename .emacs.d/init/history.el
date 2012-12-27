;; bookmark
(setq-default bookmark-default-file "~/.emacs.d/bmk")

;; recentf
(setq-default recentf-save-file
              (convert-standard-filename "~/.emacs.d/recentf"))

;; shell history
(bundle shell-history
  (setq-default shell-history-file "~/.zsh/history"))
