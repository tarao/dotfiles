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

  (defvar helm-source-ghq
    (helm-build-in-buffer-source "Repositories"
      :init #'helm-ghq:init
      :filtered-candidate-transformer #'helm-ghq:transform-candidates
      :action 'helm-ghq:action-function))

  (defun helm-ghq:root-path (roots path)
    (let ((roots (mapcar
                  #'(lambda (r)
                      (abbreviate-file-name
                       (file-name-as-directory r)))
                  roots))
          (path (abbreviate-file-name path)))
      (loop for r in roots
            until (string-prefix-p r path)
            finally return (cons r (substring path (length r))))))
  (defun helm-ghq:format (path roots)
    (let* ((root-path (helm-ghq:root-path roots path))
           (root (car root-path))
           (components (split-string (cdr root-path) "/"))
           (origin (car components))
           (repository (mapconcat #'identity (cdr components) "/")))
      (propertize
       (concat repository
               (propertize " " 'display '(space :align-to 45))
               "  "
               (propertize origin 'face 'font-lock-type-face)
               (propertize " " 'display '(space :align-to 58))
               "  "
               (propertize (format "[%s]" root)
                           'face 'font-lock-comment-face))
       'helm-realvalue (file-name-as-directory path))))
  (defun helm-ghq:transform-candidates (candidates _source)
    (let ((roots (helm-attr 'helm-ghq:roots)))
      (loop for c in candidates
            collect (helm-ghq:format c roots))))
  (defun helm-ghq:init ()
    (let ((roots (split-string
                  (shell-command-to-string "git config --get-all ghq.root")
                  "[\r\n]+")))
      (helm-attrset 'helm-ghq:roots roots)
      (with-current-buffer (helm-candidate-buffer 'global)
        (call-process-shell-command
         "ghq list -p 2>/dev/null" nil (current-buffer)))))

  (defun helm-ghq:find-files-from-directory (dir)
    (let ((default-directory dir))
      (helm-find-files-1 dir)))

  (defun helm-ghq ()
    (interactive)
    (helm :sources (list helm-source-ghq)
          :buffer "*ghq*")))

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
