;; load-path
(add-to-list 'load-path "~/.emacs.d")

;; ;; packages
;; (load "elpa-bootstrap.el")
;; (install-packages t)

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "http://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/recipes")
(require 'bundle)

(bundle tarao-elisp)

;; load init files
(bundle init-loader :url "http://gist.github.com/1021706.git"
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
  (init-loader-load "~/.emacs.d/dot"))

;; put site-lisp and its subdirectories into load-path
(when (fboundp 'normal-top-level-add-subdirs-to-load-path)
  (let* ((dir "~/.emacs.d/site-lisp")
         (default-directory dir))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir)
      (normal-top-level-add-subdirs-to-load-path))))
