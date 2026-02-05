(eval-when-compile (require 'cl-lib))

(defvar tarao/helm-for-files-basic-sources
  '(helm-source-buffers-list))
(defvar tarao/helm-for-files-dir-sources
  '(helm-source-files-in-current-dir))
(defvar tarao/helm-for-files-other-sources
  '(helm-source-recentf
    helm-source-bookmarks
    helm-source-file-cache))

(bundle helm
  (setq-default helm-truncate-lines t
                helm-allow-mouse nil
                helm-completion-mode-string ""
                helm-buffer-skip-remote-checking t
                helm-buffer-details-flag nil
                helm-move-to-line-cycle-in-source nil
                helm-for-files-preferred-list
                `(,@tarao/helm-for-files-basic-sources
                  ,@tarao/helm-for-files-dir-sources
                  ,@tarao/helm-for-files-other-sources))
  (global-set-key (kbd "C-x b") #'helm-for-files)
  (global-set-key [remap execute-extended-command] #'helm-M-x)
  (add-hook 'helm-after-initialize-hook
            #'(lambda ()
                (with-current-buffer helm-buffer
                  (setq show-trailing-whitespace nil))))

  (with-eval-after-load-feature 'helm-mode
    (define-key helm-map (kbd "M-n") #'helm-next-source)
    (define-key helm-map (kbd "M-p") #'helm-previous-source)
    (set-face-foreground 'helm-buffer-modified "#CC9393"))
  (with-eval-after-load-feature 'yaicomplete
    (add-to-list 'yaicomplete-exclude 'helm-mode))
  (helm-mode 1)

  ;; ghq

  (defvar helm-ghq:action-function 'helm-ghq:find-files-from-directory)
  (defvar helm-ghq:additional-roots
    (list (file-name-directory (directory-file-name el-get-dir))))
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
      (cl-loop for c in candidates
               collect (helm-ghq:format c root))))
  (defun helm-ghq:canonical-dir (dir)
    (file-truename (expand-file-name (file-name-as-directory dir))))
  (defun helm-ghq:roots ()
    (let ((output (shell-command-to-string "git config --get-all ghq.root")))
      (mapcar #'helm-ghq:canonical-dir
              (append (split-string output "[\r\n]+" t)
                      helm-ghq:additional-roots))))
  (defun helm-ghq:init-fun (root)
    `(lambda ()
       (let* ((root  ,root))
         (helm-attrset 'helm-ghq:root root)
         (with-current-buffer (helm-candidate-buffer 'global)
           (let ((process-environment (cons (format "GHQ_ROOT=%s" root)
                                            process-environment)))
             (make-process
              :name (format "ghq (%s)" root)
              :buffer (current-buffer)
              :command (list shell-file-name shell-command-switch "ghq list -p 2>/dev/null")
              :sentinel '(lambda (process signal)
                           (when (and (helm-window) (buffer-live-p (get-buffer helm-buffer)))
                             (with-helm-window
                               (with-current-buffer helm-buffer
                                 (let ((line (line-number-at-pos)))
                                   (helm-update)
                                   (goto-char (point-min))
                                   (forward-line (1- line))
                                   (helm-skip-noncandidate-line 'next)
                                   (helm-mark-current-line)
                                   (helm-display-mode-line (helm-get-current-source))))))
                           (when (memq (process-status process) '(exit signal))
                             (shell-command-set-point-after-cmd (process-buffer process))))))))))
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
    (require 'helm-for-files)
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
            (if (helm-ghq-wt:should-show-worktrees-p dir)
                ;; Show worktree selection
                (helm-ghq-wt:select-worktree dir #'helm-ghq-wt:default-action)
              ;; Use default action
              (helm-ghq-wt:default-action dir)))))

  ;; git-wt worktree selection integration

  (defun helm-ghq-wt:parse-worktree-line (line)
    "Parse a worktree line and return (display . path) cons cell."
    (when (string-match "^\\*?[[:space:]]*\\([^[:space:]]+\\)[[:space:]]+\\([^[:space:]]+\\)" line)
      (let ((path (match-string 1 line))
            (branch (match-string 2 line)))
        (cons (concat branch
                      (propertize " " 'display '(space :align-to 60))
                      "  "
                      (propertize path 'face 'font-lock-comment-face))
              path))))

  (defun helm-ghq-wt:get-worktrees (dir)
    "Get list of worktrees for DIR as helm candidates."
    (let* ((default-directory dir)
           (output (shell-command-to-string "git-wt 2>/dev/null"))
           (lines (split-string output "[\r\n]+" t)))
      ;; Remove header line
      (when (> (length lines) 1)
        (setq lines (cdr lines)))
      ;; Parse each line
      (delq nil (mapcar #'helm-ghq-wt:parse-worktree-line lines))))

  (defun helm-ghq-wt:should-show-worktrees-p (dir)
    "Return t if DIR has multiple worktrees and git-wt is available."
    (and (executable-find "git-wt")
         (let* ((default-directory dir)
                (output (shell-command-to-string "git-wt 2>/dev/null"))
                (lines (split-string output "[\r\n]+" t)))
           ;; Need at least 3 lines: header + 2 worktrees
           (>= (length lines) 3))))

  (defun helm-ghq-wt:select-worktree (dir action)
    "Show worktree selection for DIR and call ACTION with selected path."
    (let ((candidates (helm-ghq-wt:get-worktrees dir)))
      (if (null candidates)
          ;; No worktrees, use default action
          (funcall action dir)
        ;; Show worktree selection
        (helm :sources
              (helm-build-sync-source "Worktrees"
                :candidates candidates
                :action action)
              :buffer "*helm ghq worktrees*"))))

  (defun helm-ghq-wt:default-action (dir)
    "Default action: open helm-git-files or helm-find-files in DIR."
    (require 'helm-git-files)
    (require 'helm-files)
    (let ((default-directory dir))
      (if (helm-git-files:git-p)
          (helm-git-files)
        (helm-find-files-1 dir)))))

(bundle helm-descbinds
  (helm-descbinds-mode))

(bundle helm-git-grep)
