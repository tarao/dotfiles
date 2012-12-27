;; hostname
(string-match "^\\([^\\.]+\\)\\(\\.\\(.*\\)\\)?$" (system-name))
(defconst short-hostname (replace-match "\\1" t nil (system-name))
  "Host part of function `system-name'.")

;; shell
(setq-default explicit-shell-file-name "zsh")
(setq shell-file-name "zsh"
      shell-command-switch "-c")

;; interactive
(fset 'yes-or-no-p 'y-or-n-p)
