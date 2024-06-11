(when (executable-find "xdg-open")
  (with-eval-after-load-feature 'browse-url
    (setq browse-url-generic-program "xdg-open"
          browse-url-browser-function 'browse-url-generic)))

(defun wsl-browse-url (url &rest args)
  (prog1 (message "Open %s" url)
    (shell-command-to-string
     (mapconcat #'shell-quote-argument
                (list "cmd.exe" "/c" "start" url)
                " "))))

(when (getenv "WSL_DISTRO_NAME")
  (with-eval-after-load-feature 'browse-url
    (setq browse-url-browser-function #'wsl-browse-url)))

