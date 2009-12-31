(setq load-path (cons "/usr/local/share/emacs/site-lisp/ddskk" load-path))
(load "~/.emacs.d/dot/common")
(load "dot/tarao")

;; edit textarea
(require 'textarea)
(setq textarea-dir "~/win/var")
