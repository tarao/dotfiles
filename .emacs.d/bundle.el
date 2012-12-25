(require 'el-get)
(eval-when-compile (require 'cl))

(defgroup bundle nil "bundle"
  :group 'convenience)

(defcustom bundle-byte-compile t
  "t means to automatically byte-compile init forms."
  :type 'boolean
  :group 'bundle)

(defcustom bundle-init-directory "~/.emacs.d/bundle/init/"
  "Directory to save auto generated init files."
  :type 'directory
  :group 'bundle)

(defvar bundle-inits nil)

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
    source))

(defun bundle-init-id (&rest args)
  (let* ((key (mapconcat #'(lambda (x) (format "%s" x)) args ";"))
         (pair (assoc key bundle-inits)))
    (if pair
        (setcdr pair (1+ (cdr pair)))
      (push (cons key 1) bundle-inits)
      1)))

(defun bundle-load-init (el)
  (let ((lib (file-name-sans-extension el))
        (elc (concat el "c")))
    (when (or (not (file-exists-p elc))
              (file-newer-than-file-p el elc))
      (byte-compile-file el))
    (load (expand-file-name lib))))

(defun bundle-make-init (src)
  (when (and bundle-byte-compile
             (plist-get src :after)
             load-file-name
             (condition-case nil
                 (or (file-exists-p bundle-init-directory)
                     (make-directory bundle-init-directory t) t)
               (error nil)))
    (let* ((path (file-name-sans-extension (expand-file-name load-file-name)))
           (path (split-string path "/"))
           (call-site (mapconcat #'identity path "_"))
           (package (plist-get src :name))
           (id (bundle-init-id package call-site))
           (init-file (concat bundle-init-directory
                              (format "%s_%s-%d" package call-site id)))
           (el (concat init-file ".el"))
           (form (plist-get src :after)))
      ;; generate .el file
      (when (or (not (file-exists-p el))
                (file-newer-than-file-p load-file-name el))
        (with-temp-buffer
          (if (listp form)
              (dolist (exp form) (pp exp (current-buffer)))
            (pp form (current-buffer)))
          (write-region nil nil el)))

      ;; loader
      `((bundle-load-init ,el)))))

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

    ;; entering password via process-filter only works in async mode
    (when (or (and (eq (plist-get def :type) 'cvs)
                   (eq (plist-get def :options) 'login)
                   (not (string-match-p "^:pserver:.*:.*@.*:.*$"
                                        (or (plist-get def :url) ""))))
              (eq (plist-get def :type) 'apt)
              (eq (plist-get def :type) 'fink)
              (eq (plist-get def :type) 'pacman))
      (setq sync nil))

    ;; byte-compile :after script
    (let ((form  (or (bundle-make-init def) (plist-get def :after))))
      (when form
        (setq def (plist-put def :after `(progn ,@form)))))

    ;; get
    (add-to-list 'el-get-sources def)
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
