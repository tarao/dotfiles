;; bookmark
(setq-default bookmark-default-file (locate-user-emacs-file "bmk"))

;; recentf
(setq-default recentf-save-file (locate-user-emacs-file "recentf"))

;; shell history
(bundle shell-history
  (setq-default shell-history-file "~/.zsh/history"))
