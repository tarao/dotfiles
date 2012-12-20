;; bookmark
(setq bookmark-default-file "~/.emacs.d/bmk")

;; recentf
(setq recentf-save-file (convert-standard-filename "~/.emacs.d/recentf"))

;; shell history
(bundle shell-history
  (setq shell-history-file "~/.zsh/history"))
