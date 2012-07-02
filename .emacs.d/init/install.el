(setq elisp-emacswiki-src
      '(
        "install-elisp.el"
        "auto-install.el"
        "anything.el"
        "anything-match-plugin.el"
        "anything-config.el"
        "anything-obsolete.el"
        "anything-complete.el"
        "anything-grep.el"
        "browse-kill-ring.el"
        "browse-kill-ring+.el"
        "c-eldoc.el"
        "csharp-mode.el"
        "descbinds-anything.el"
        "eldoc-extension.el"
        "goto-chg.el"
        "key-chord.el"
        "moccur.el"
        "color-moccur.el"
        "shell-history.el"
        "space-chord.el"
        "xquery-mode.el"
        ))
(setq elisp-url-src
      '(
        "http://www.brgeight.se/downloads/emacs/css-mode.el"
        "http://svn.coderepos.org/share/lang/elisp/init-loader/init-loader.el"
        "http://homepage3.nifty.com/satomii/software/jaspace.el"
        "http://www.brgeight.se/downloads/emacs/javascript.el"
        "http://www.loveshack.ukfsn.org/emacs/multi-mode.el"
        "http://taiyaki.org/elisp/word-count/src/word-count.el"
        ))

(if (eq (user-real-uid) 0)
    ;;; root (install to the system directory)
    (progn
      (defun head3 (l n)
        (if (null l) l (if (< n 3) (cons (car l) (head3 (cdr l) (1+ n))) nil)))
      (let ((vl (head3 (split-string emacs-version "\\.") 0))
            (dir "/usr/local/share/emacs/%s/site-lisp/"))
        (setq install-elisp-repository-directory
              (format dir (mapconcat 'identity vl ".")))))
  ;;; user (install to the user directory)
  (progn
    (setq load-path (cons "~/.emacs.d/site-lisp" load-path))
    (setq install-elisp-repository-directory "~/.emacs.d/site-lisp/")))

(require 'install-elisp)

(defadvice install-elisp (before batch-install-elisp activate)
  (when noninteractive (setq install-elisp-confirm-flag nil)))
(defun update-remote-emacs-lisp ()
  (interactive)
  (when noninteractive (setq install-elisp-confirm-flag nil))
  (dolist (src elisp-emacswiki-src)
    (when noninteractive (sleep-for 2))
    (install-elisp-from-emacswiki src))
  (dolist (src elisp-url-src) (install-elisp src)))
