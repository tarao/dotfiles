;; load-path
(setq load-path (cons "~/.emacs.d" load-path))
(when (fboundp 'normal-top-level-add-subdirs-to-load-path)
  (let* ((dir "~/.emacs.d/site-lisp")
         (default-directory dir))
    (when (file-directory-p dir)
      (setq load-path (cons dir load-path))
      (normal-top-level-add-subdirs-to-load-path))))
(setq load-path (cons "~/.emacs.d/auto-install" load-path))

;; load init files
(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(defadvice init-loader--re-load-files
  (around ad-init-loader-filter-backup-files activate)
  "Don't load file whose name ends with '~'."
  (let ((filter '(lambda (re l)
                   (and (consp l)
                        (let ((h (car l)) (rest (funcall filter re (cdr l))))
                          (if (string-match re h) rest (cons h rest))))))
        (l ad-do-it))
    (setq ad-return-value (funcall filter "~$" l))))
(init-loader-load "~/.emacs.d/dot")
