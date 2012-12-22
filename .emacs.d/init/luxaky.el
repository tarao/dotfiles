(when (equal short-hostname "luxaky")
  ;; mew
  (autoload 'mew "mew" nil t)
  (autoload 'mew-send "mew" nil t)
  (setq mew-smtp-user (user-login-name)
        mew-mail-domain "orezdnu.org"
        mew-mailbox-type 'mbox
        mew-mbox-command "incm"
        mew-mbox-command-arg
        (concat "-u -d /home/users/" (user-login-name) "/Maildir"))
  ;; emacs-w3m - a text browser
  (setq w3m-init-file "dot/.emacs-w3m")
  (add-hook 'w3m-mode-hook
            '(lambda () (setq show-trailing-whitespace nil)))
  ;; lookup - search dictionary
  (autoload 'lookup "lookup" nil t)
  (autoload 'lookup-region "lookup" nil t)
  (autoload 'lookup-pattern "lookup" nil t)
  (global-set-key (kbd "C-x C-y") 'lookup)
  (global-set-key (kbd "C-x y") 'lookup-region)
  (global-set-key (kbd "C-x l") 'lookup-pattern))
