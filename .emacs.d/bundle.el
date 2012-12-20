(require 'el-get)
(eval-when-compile (require 'cl))

;; patch
(defadvice el-get-update-autoloads
  (around bundle-respect-autoloads (package) activate)
  "Suppress generating autoloads if \":autoloads nil\" is specified.
This is a bug in el-get and will be fixed in 5.0. See
https://github.com/dimitri/el-get/issues/810 for details."
  (let ((def (el-get-package-def package)))
    (unless (and (plist-member def :autoloads)
                 (not (plist-get def :autoloads)))
      ad-do-it)))

(defun bundle-defined-p (src)
  (condition-case nil
      (el-get-package-def (if (listp src) (el-get-source-name src) src))
    (error nil)))

(defun bundle-guess-type (src)
  (cond
   ((plist-member src :url)
    (let ((url (plist-get src :url)))
      (cond
       ((or (string-match-p "^git:" url) (string-match-p "\\.git$" url))
        'git)
       ((or (string-match-p "^bzr:" url) (string-match-p "^lp:" url))
        'bzr)
       ((string-match-p "^svn:" url)
        'svn)
       ((string-match-p ":pserver:" url)
        'cvs)
       ((string-match-p "ftp://" url)
        'ftp)
       ((or (string-match-p "https?://" url) (string-match-p "\\.el$" url))
        'http))))
   (t 'elpa)))

(defun bundle-merge-source (src)
  (let* ((name (el-get-source-name src))
         (source (if (plist-get src :type) nil (el-get-package-def name))))
    (while (keywordp (nth 0 src))
      (setq source (plist-put source (nth 0 src) (nth 1 src))
            src (cdr-safe (cdr src))))
    (add-to-list 'el-get-sources source)))

(defmacro bundle (feature &rest form)
  "Require FEATURE and run init script specified by FORM.

FORM may be started with a property list. In that case, the
property list is pushed to `el-get-sources'.

The rest of FORM is evaluated after FEATURE is loaded."
  (declare (indent defun) (debug t))
  (let* ((featureq (or (and (consp feature) (eq (nth 0 feature) 'quote) feature)
                       (list 'quote feature)))
         (feature (or (and (listp feature) (nth 1 feature)) feature))
         package src body after)
    (while (keywordp (nth 0 form))
      ;; parse keywords
      (setq src (plist-put src (nth 0 form) (nth 1 form))
            form (cdr-safe (cdr form))))
    (unless (plist-member src :name)
      ;; default name
      (setq src (plist-put src :name feature)))
    (unless (or (plist-member src :features)
                (and (eq (plist-get src :name) feature)
                     (bundle-defined-p src)))
      ;; put default feature
      (setq src (plist-put src :features feature)))
    (unless (or (plist-member src :type) (bundle-defined-p src))
      ;; put default type
      (setq src (plist-put src :type (bundle-guess-type src))))
    (setq package (list 'quote (plist-get src :name)))
    (when form (setq src (plist-put src :after `(progn ,@form))))
    `(progn
       (when ',src (bundle-merge-source ',src))
       (let ((def (el-get-package-def (el-get-source-name ',src))) (sync t))
         (when (or (and (eq (plist-get def :type) 'cvs)
                        (eq (plist-get def :options) 'login))
                   (eq (plist-get def :type) 'apt)
                   (eq (plist-get def :type) 'fink)
                   (eq (plist-get def :type) 'pacman))
           ;; entering password via process-filter only works in async mode
           (setq sync nil))
         (el-get sync ,package)))))

(provide 'bundle)
