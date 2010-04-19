(setq howm-directory "~/docs/howm/")
(setq howm-file-name-format "%Y/%m/%Y-%m-%dT%H%M%S.howm")
(setq howm-keyword-file "~/docs/.howm-keys")
(setq howm-menu-lang 'en)
(setq auto-mode-alist (append '(("\\.howm$" . rdoc-mode)) auto-mode-alist))

(require 'howm)
