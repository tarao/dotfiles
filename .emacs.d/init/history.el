;; bookmark
(setq-default bookmark-default-file (user-emacs-directory-file "bmk"))

;; recentf
(setq-default recentf-save-file (user-emacs-directory-file "recentf"))

;; shell history
(bundle shell-history
  (setq-default shell-history-file "~/.zsh/history"))
