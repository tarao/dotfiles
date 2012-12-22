;; edit textarea
(require 'textarea)
(setq textarea-dir "~/win/var")
(defadvice textarea (after textarea-text-major-mode activate) (text-mode))
(add-to-list 'end-mark-mode-buffers-regexp "^\\*textarea\\*$")
