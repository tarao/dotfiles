(add-to-list 'load-path "~/.emacs.d")

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "http://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")

;; bundle - an el-get wrapper
(add-to-list 'el-get-sources
             '(:name bundle
                     :url "http://gist.github.com/4414297.git"
                     :type git
                     :features (bundle)))
(el-get 'sync 'bundle)

;; byte-compiling version of eval-after-load
(bundle! gist:4414304:eval-after-load-compile)

;; load init files
(bundle! gist:4362564:init-loader
  ;; load
  (setq-default init-loader-show-log-after-init nil)
  (init-loader-load "~/.emacs.d/dot")

  ;; hide compilation results
  (let ((win (get-buffer-window "*Compile-Log*")))
    (when win (delete-window win))))

;; put site-lisp and its subdirectories into load-path
(when (fboundp 'normal-top-level-add-subdirs-to-load-path)
  (let* ((dir "~/.emacs.d/site-lisp")
         (default-directory dir))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir)
      (normal-top-level-add-subdirs-to-load-path))))
