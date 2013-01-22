;; emacs directory
(defsubst user-emacs-directory-file (file)
  (concat (file-name-as-directory user-emacs-directory) file))
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(add-to-list 'load-path user-emacs-directory)

;; el-get
(add-to-list 'load-path (user-emacs-directory-file "el-get/el-get"))
(setq-default el-get-dir (user-emacs-directory-file "el-get")
              el-get-emacswiki-base-url
              "http://raw.github.com/emacsmirror/emacswiki.org/master/")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "http://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(add-to-list 'el-get-recipe-path (user-emacs-directory-file "recipes"))

;; bundle - an el-get wrapper
(add-to-list 'el-get-sources
             '(:name bundle :type github :pkgname "tarao/bundle-el"))
(el-get 'sync 'bundle)

;; load init files
(bundle! gist:4362564:init-loader
  ;; load
  (setq-default init-loader-show-log-after-init nil)
  (init-loader-load (user-emacs-directory-file "dot"))

  ;; hide compilation results
  (let ((win (get-buffer-window "*Compile-Log*")))
    (when win (delete-window win))))

;; put site-lisp and its subdirectories into load-path
(when (fboundp 'normal-top-level-add-subdirs-to-load-path)
  (let* ((dir (user-emacs-directory-file "site-lisp"))
         (default-directory dir))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir)
      (normal-top-level-add-subdirs-to-load-path))))
