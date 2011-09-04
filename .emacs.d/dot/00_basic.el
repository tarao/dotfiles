;; hostname
(string-match "^\\([^\\.]+\\)\\(\\.\\(.*\\)\\)?$" (system-name))
(defconst short-hostname (replace-match "\\1" t nil (system-name))
  "Host part of function `system-name'.")

;; shell
(setq explicit-shell-file-name "zsh")
(setq shell-file-name "zsh")
(setq shell-command-switch "-c")
