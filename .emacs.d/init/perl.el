(eval-when-compile (require 'cl))

(defconst perl-mode-files '("\\.pl$" "\\.pm$" "\\.t$"))

;; auto-mode
(setq auto-mode-alist
      (append (mapcar #'(lambda (x) (cons x 'cperl-mode)) perl-mode-files)
              auto-mode-alist))
(fset 'perl-mode 'cperl-mode)

;; syntax
(setq-default cperl-indent-level 4
              cperl-continued-statement-offset 4
              cperl-close-paren-offset -4
              cperl-comment-column 40
              cperl-highlight-variables-indiscriminately t
              cperl-indent-parens-as-block t
              cperl-label-offset -4
              cperl-tab-always-indent nil
              cperl-font-lock t)

;; auto-insert package template
(add-to-list 'auto-insert-alist '("\\.pm$" . ["insert.pm" template-replacer]))
(add-to-list 'template-replacement-alist '("%Perl-Package%" . pm2package))
(defun last-member (item list)
  (let ((right (member item list)))
    (and right (or (last-member item (cdr right)) right))))
(defun pm2package (&optional fname)
  (unless fname (setq fname (buffer-file-name)))
  (let* ((lib (cdr (last-member "lib" (split-string fname "/"))))
         (package (mapconcat #'identity lib "::")))
      (replace-regexp-in-string "\\.pm$" "" package)))

;; set PERL5LIB

(defun findlib (lib &optional path root multi)
  "Find LIB in ancestors of PATH until ROOT.
If MULTI is non-nil, then all occurrence of LIB in the ancestors
are returned (as a list).  Otherwise, only the first occurrence
is returned (as a single string)."
  (unless path (setq path (buffer-file-name)))
  (unless root (setq root "/"))
  (let ((root (file-name-as-directory (expand-file-name root)))
        (path (file-name-directory (expand-file-name path))) found)
    (while (and path (or multi (not found)))
      (let ((candidate (concat path lib)))
        (when (file-readable-p candidate) (push candidate found)))
      (setq path (if (or (string= path root) (string= path "/")) nil
                   (file-name-directory (directory-file-name path)))))
    (if multi
        found
      (and found (car found)))))
(defsubst findlibs (lib &optional path root)
  (findlib lib path root t))

(defun shell-command-to-string-with-directory (dir cmd)
  "The same as `shell-command-to-string' but set DIR as the
process's current directory."
  (with-output-to-string
    (with-current-buffer standard-output
      (let ((default-directory dir))
        (process-file shell-file-name nil t nil shell-command-switch cmd)))))

(autoload 'vc-git-root "vc-git")

(defun git-submodule-dirs (&optional root)
  "List submodule directories of a git repository ROOT.
If ROOT is omitted, then the repository root of the buffer file
is used."
  (unless root (setq root (vc-git-root (buffer-file-name))))
  (let* ((root (file-name-as-directory root))
         (cmd "git submodule status")
         (status (shell-command-to-string-with-directory root cmd))
         (prefix-length 42))
    (loop for l in (split-string status "\n")
          for status = (replace-regexp-in-string " (.*)$" "" l)
          when (> (length status) prefix-length)
          collect (concat root (substring status prefix-length)))))

(defun git-findlib (lib &optional path nosubmodule)
  "Find LIB under the root of the git repository of PATH.
If PATH is omitted, then the repository root of the buffer file
is used.  Submodule directories are searched unless NOSUBMODULE
is non-nil."
  (unless path (setq path (buffer-file-name)))
  (let* ((path (expand-file-name path))
         (root (file-name-as-directory (vc-git-root path)))
         (libs (findlib lib root root)))
    (when libs (setq libs (list libs)))
    (loop for dir in (and (not nosubmodule) (git-submodule-dirs root))
          for dir = (file-name-as-directory dir)
          for sublib = (findlib lib dir dir)
          when sublib collect sublib into sublibs
          finally return (append libs sublibs))))

(defun git-perl-lib (lib path)
  (let* ((root (file-name-as-directory (expand-file-name (vc-git-root path))))
         (sym (intern root)))
    (or (get sym :perl-lib)
        (put sym :perl-lib (git-findlib lib path)))))

(defvar perl-lib nil)
(make-variable-buffer-local 'perl-lib)
(defconst perl-lib-env-name "PERL5LIB")

(defun add-perl-lib (&rest libs)
  "Add directory names in LIBS to Perl library paths."
  (let* ((path (expand-file-name (or (buffer-file-name) default-directory)))
         (git-root (vc-git-root path)) (root (or git-root "/")))
    (dolist (lib libs)
      (setq perl-lib
            (append perl-lib
                    (and git-root (git-perl-lib lib path))
                    (findlibs lib path root)))))
  perl-lib)

(defun update-perl-lib (libs)
  (let ((paths (split-string (or (getenv perl-lib-env-name) "") ":")))
    (dolist (l libs) (unless (member l paths) (push l paths)))
    (setenv perl-lib-env-name (mapconcat #'identity paths ":"))))

(defadvice flymake-start-syntax-check-process
  (around set-perl-lib activate)
  "Set PERL5LIB environment variable before starting a check process.
The old value of the environment variable is restored after
invoking the process."
  (if perl-lib
      (let ((oldenv (getenv perl-lib-env-name)))
        (update-perl-lib perl-lib)
        ad-do-it
        (setenv perl-lib-env-name oldenv)) ; restore
    ad-do-it))

;; flymake

(defvar flymake-perl-err-line-patterns
  '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))
(defconst flymake-allowed-perl-files
  (cons "\\.cgi$" perl-mode-files))
(defconst flymake-allowed-perl-file-name-masks
  (mapcar #'(lambda (x) (list x 'flymake-perl-init))
          flymake-allowed-perl-files))

(autoload 'flymake-init-create-temp-buffer-copy "flymake")
(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc" local-file))))

(eval-after-load-compile 'flymake
  (setq flymake-allowed-file-name-masks
        (append flymake-allowed-file-name-masks
                flymake-allowed-perl-file-name-masks)))

(add-hook 'cperl-mode-hook
          #'(lambda ()
              (add-perl-lib "lib")
              (flymake-mode 1)
              (when (boundp 'flymake-err-line-patterns)
                (set (make-local-variable 'flymake-err-line-patterns)
                     flymake-perl-err-line-patterns))))
