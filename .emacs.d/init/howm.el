(bundle howm
  (setq-default howm-directory "~/docs/howm/"
                howm-file-name-format "%Y/%m/%Y-%m-%dT%H%M%S.howm"
                howm-keyword-file "~/docs/howm/keys"
                howm-history-file "~/docs/howm/history"
                howm-menu-lang 'en)
  (setq auto-mode-alist (append '(("\\.howm$" . rdoc-mode)) auto-mode-alist))
  (global-set-key (kbd "C-c , ,") #'howm-menu))
