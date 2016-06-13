(defvar tarao/helm-for-files-basic-sources
  '(helm-source-buffers-list))
(defvar tarao/helm-for-files-dir-sources
  '(helm-source-files-in-current-dir))
(defvar tarao/helm-for-files-other-sources
  '(helm-source-recentf
    helm-source-bookmarks
    helm-source-file-cache
    helm-source-locate))

(bundle helm
  (setq-default helm-truncate-lines t
                helm-completion-mode-string ""
                helm-buffer-skip-remote-checking t
                helm-buffer-details-flag nil
                helm-for-files-preferred-list
                `(,@tarao/helm-for-files-basic-sources
                  ,@tarao/helm-for-files-dir-sources
                  ,@tarao/helm-for-files-other-sources))
  (global-set-key (kbd "C-x b") #'helm-for-files)
  (global-set-key [remap execute-extended-command] #'helm-M-x)

  (with-eval-after-load-feature 'helm-mode
    (define-key helm-map (kbd "M-n") #'helm-next-source)
    (define-key helm-map (kbd "M-p") #'helm-previous-source))
  (with-eval-after-load-feature 'yaicomplete
    (add-to-list 'yaicomplete-exclude 'helm-mode))
  (helm-mode 1)

  ;; ghq

  (defvar helm-ghq:action-function 'helm-ghq:find-files-from-directory)
  (defun helm-ghq:rel-path (root path)
    (let ((root (abbreviate-file-name root))
          (path (abbreviate-file-name path)))
      (substring path (length root))))
  (defun helm-ghq:format (path root)
    (let* ((components (split-string (helm-ghq:rel-path root path) "/"))
           (origin (car components))
           (repository (mapconcat #'identity (cdr components) "/")))
      (propertize
       (concat repository
               (propertize " " 'display '(space :align-to 60))
               "  "
               (propertize origin 'face 'font-lock-type-face))
       'helm-realvalue (file-name-as-directory path))))
  (defun helm-ghq:transform-candidates (candidates _source)
    (let ((root (helm-attr 'helm-ghq:root)))
      (loop for c in candidates
            collect (helm-ghq:format c root))))
  (defun helm-ghq:roots ()
    (let ((output (shell-command-to-string "git config --get-all ghq.root")))
      (mapcar #'(lambda (r) (expand-file-name (file-name-as-directory r)))
              (split-string output "[\r\n]+" t))))
  (defun helm-ghq:init-fun (root)
    `(lambda ()
       (let* ((root  ,root))
         (helm-attrset 'helm-ghq:root root)
         (with-current-buffer (helm-candidate-buffer 'global)
           (let ((process-environment (cons (format "GHQ_ROOT=%s" root)
                                            process-environment)))
             (call-process-shell-command
              "ghq list -p 2>/dev/null" nil (current-buffer)))))))
  (defun helm-ghq:source (root)
    (let ((source-name (abbreviate-file-name (directory-file-name root))))
      (helm-build-in-buffer-source (format "Repositories in %s" source-name)
        :init (helm-ghq:init-fun root)
        :filtered-candidate-transformer #'helm-ghq:transform-candidates
        :fuzzy-match t
        :persistent-action 'ignore
        :action 'helm-ghq:action-function)))
  (defun helm-ghq:find-files-from-directory (dir)
    (let ((default-directory dir))
      (helm-find-files-1 dir)))
  (defun helm-ghq ()
    (interactive)
    (let ((roots (helm-ghq:roots)))
      (helm :sources (mapcar #'helm-ghq:source roots)
            :buffer "*ghq*"))))

(bundle helm-git-files
  (defun tarao/helm-for-files ()
    (interactive)
    (require 'helm-git-files)
    (unless helm-source-buffers-list
      (setq helm-source-buffers-list
            (helm-make-source "Buffers" 'helm-source-buffers)))
    (let* ((git-sources (if (helm-git-files:git-p)
                            `(helm-git-files:modified-source
                              helm-git-files:untracked-source
                              helm-git-files:all-source
                              ,@(helm-git-files:submodule-sources 'all))
                          tarao/helm-for-files-dir-sources))
           (sources `(,@tarao/helm-for-files-basic-sources
                      ,@git-sources
                      ,@tarao/helm-for-files-other-sources)))
      (helm :sources sources
            :ff-transformer-show-only-basename t
            :buffer "*helm for files*")))

  (setq helm-ghq:action-function
        #'(lambda (dir)
            (require 'helm-git-files)
            (require 'helm-files)
            (let ((default-directory dir))
              (if (helm-git-files:git-p)
                  (helm-git-files)
                (helm-find-files-1 dir))))))

(bundle helm-descbinds
  (helm-descbinds-mode))

(bundle helm-git-grep)
