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

(defun bundle-package-def (src)
  (condition-case nil
      (el-get-package-def (if (listp src) (el-get-source-name src) src))
    (error nil)))
(defalias 'bundle-defined-p (symbol-function 'bundle-package-def))

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
    (add-to-list 'el-get-sources source)
    source))

(defun bundle-el-get (src)
  (let ((package (plist-get src :name)) (def (bundle-package-def src))
        (fs (plist-get src :features)) (sync 'sync))
    ;; merge features
    (when (plist-member def :features)
      (let* ((old (plist-get def :features))
             (old (or (and (listp old) old) (list old))))
        (dolist (f old) (add-to-list 'fs f))
        (setq src (plist-put src :features fs))))
    ;; merge src with the oriiginal definition
    (setq def (bundle-merge-source src))

    (when (or (and (eq (plist-get def :type) 'cvs)
                   (eq (plist-get def :options) 'login)
                   (not (string-match-p "^:pserver:.*:.*@.*:.*$"
                                        (plist-get def :url))))
              (eq (plist-get def :type) 'apt)
              (eq (plist-get def :type) 'fink)
              (eq (plist-get def :type) 'pacman))
      ;; entering password via process-filter only works in async mode
      (setq sync nil))

    (el-get sync package)))

(defmacro bundle (feature &rest form)
  "Install FEATURE and run init script specified by FORM.

FORM may be started with a property list. In that case, the
property list is pushed to `el-get-sources'.

The rest of FORM is evaluated after FEATURE is loaded."
  (declare (indent defun) (debug t))
  (let* ((feature (or (and (listp feature) (nth 1 feature)) feature))
         src require)
    ;; (bundle FEATURE in PACKAGE ...) form
    (when (eq (nth 0 form) 'in)
      (let* ((name (nth 1 form))
             (name (or (and (listp name) (nth 1 name)) name)))
        (setq src (plist-put src :name name)))
      (setq form (nthcdr 2 form) require t))
    ;; parse keywords
    (while (keywordp (nth 0 form))
      (setq src (plist-put src (nth 0 form) (nth 1 form))
            form (cdr-safe (cdr form))))
    ;; package name
    (unless (plist-member src :name) (setq src (plist-put src :name feature)))
    ;; put default type
    (unless (or (plist-member src :type) (bundle-defined-p src))
      (setq src (plist-put src :type (bundle-guess-type src))))
    ;; features
    (when (plist-member src :features)
      (let* ((fs (plist-get src :features))
             (fs (or (and (listp fs) fs) (list fs))))
        (setq src (plist-put src :features fs))))
    (when (and require (or (not (plist-member src :features))
                           (plist-get src :features)))
      ;; put the feature into the features list
      (let ((fs (plist-get src :features)))
        (add-to-list 'fs feature)
        (setq src (plist-put src :features fs))))
    ;; init script
    (when form (setq form `(progn ,@form)))
    (setq src (plist-put src :after form))

    `(bundle-el-get ',src)))

(defmacro bundle! (feature &rest args)
  "Install FEATURE and run init script.
It is the same as `bundle' except that FEATURE is explicitly
required."
  (if (eq (nth 0 args) 'in)
      `(bundle ,feature ,@args)
    `(bundle ,feature ,@(list* 'in feature args))))

(provide 'bundle)
