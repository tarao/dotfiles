(require 'install-elisp)

(setq elisp-emacswiki-src
      '(
        "install-elisp.el"
        "anything.el"
        "anything-config.el"
        "browse-kill-ring.el"
        "browse-kill-ring+.el"
        "descbinds-anything.el"
        "xquery-mode.el"
        ))
(setq elisp-url-src
      '(
        "http://www.brgeight.se/downloads/emacs/css-mode.el"
        "http://homepage3.nifty.com/satomii/software/jaspace.el"
        "http://www.brgeight.se/downloads/emacs/javascript.el"
        "http://taiyaki.org/elisp/word-count/src/word-count.el"
        ))

(if (eq (user-real-uid) 0)
    ;;; root (install to the system directory)
    (progn
      (defun head3 (l n)
        (if (null l) l (if (< n 3) (cons (car l) (head3 (cdr l) (1+ n))) nil)))
      (let ((vl (head3 (split-string emacs-version "\\.") 0))
            (dir "/usr/local/share/emacs/%s/site-lisp-test/"))
        (setq install-elisp-repository-directory
              (format dir (mapconcat 'identity vl ".")))))
  ;;; user (install to the user directory)
  (setq install-elisp-repository-directory "~/.emacs.d/site-lisp/"))

(defun update-remote-emacs-lisp ()
  (interactive)
  (when noninteractive (setq install-elisp-confirm-flag nil))
  (mapcar 'install-elisp-from-emacswiki elisp-emacswiki-src)
  (mapcar 'install-elisp elisp-url-src))
