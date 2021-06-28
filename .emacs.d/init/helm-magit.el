(eval-when-compile (require 'cl-lib))

(bundle helm)
(bundle magit
  (autoload 'magit-log-mode "magit-log")
  (define-derived-mode magit-for-each-ref-mode magit-log-mode "Magit Refs"
    "Mode for showing the commit log for each ref."
    :group 'magit-log
    (hack-dir-local-variables-non-file-buffer))

  (define-derived-mode magit-for-each-local-branche-mode magit-for-each-ref-mode
    "Magit Local Branches"
    "Mode for showing the latest commit of local branches."
    :group 'magit-log
    (hack-dir-local-variables-non-file-buffer))

  (define-derived-mode magit-for-each-remote-branche-mode magit-for-each-ref-mode
    "Magit Remote Branches"
    "Mode for showing the latest commit of remote branches."
    :group 'magit-log
    (hack-dir-local-variables-non-file-buffer))

  (define-derived-mode magit-for-each-tag-mode magit-for-each-ref-mode
    "Magit Tags"
    "Mode for showing the latest commit of tags."
    :group 'magit-log
    (hack-dir-local-variables-non-file-buffer))

  (defun magit-for-each-ref-refresh-buffer (refs)
    (eval-and-compile (require 'magit-log))
    (magit-insert-section (logbuf)
      (magit-git-wash (apply-partially #'magit-log-wash-log 'log)
        "for-each-ref"
        "--sort=-authordate"
        "--format=%(objectname:short) (%(refname), %(upstream)) [%(authorname)][%(authordate)]%(contents:subject)"
        refs)))

  (defun magit-for-each-local-branche-refresh-buffer ()
    (magit-for-each-ref-refresh-buffer "refs/heads"))

  (defun magit-for-each-remote-branche-refresh-buffer ()
    (magit-for-each-ref-refresh-buffer "refs/remotes"))

  (defun magit-for-each-tag-refresh-buffer ()
    (magit-for-each-ref-refresh-buffer "refs/tags"))

  (defun magit-refs-local-branches ()
    (interactive)
    (magit-mode-setup #'magit-for-each-local-branche-mode))

  (defun magit-refs-remote-branches ()
    (interactive)
    (magit-mode-setup #'magit-for-each-remote-branche-mode))

  (defun magit-refs-tags ()
    (interactive)
    (magit-mode-setup #'magit-for-each-tag-mode))

  (defmacro helm-magit:init (&rest body)
    `(progn
       (eval-and-compile (require 'magit-log))
       (let ((buffer (progn ,@body
                          (prog1 (current-buffer)
                            (magit-log-bury-buffer 1)))))
       (helm-candidate-buffer buffer))))

  (defun helm-magit:local-branches-init ()
    (helm-magit:init
     (magit-refs-local-branches)))

  (defun helm-magit:remote-branches-init ()
    (helm-magit:init
     (magit-refs-remote-branches)))

  (defun helm-magit:tags-init ()
    (helm-magit:init
     (magit-refs-tags)))

  (defun helm-magit:head-commits-init ()
    (helm-magit:init
     (magit-log-head (remove "--graph" magit-log-arguments))))

  (defun helm-magit:log-transform-candidates (candidates _source)
    (eval-and-compile (require 'magit-section))
    (cl-loop for c in candidates
             unless (string= c "(empty)")
             collect (let* ((section (get-text-property 0 'magit-section c))
                            (commit (magit-section-value section)))
                       (propertize c 'helm-realvalue commit))))

  (defun helm-magit:commit-shortname (commit)
    (eval-and-compile (require 'magit-git))
    (or (magit-name-local-branch commit)
        (magit-name-remote-branch commit)
        (magit-name-tag commit)
        (magit-get-shortname commit)
        commit))

  (defclass helm-magit:source-log (helm-source-in-buffer)
    ((get-line
      :initform #'buffer-substring)
     (fuzzy-match
      :initform t)
     (filtered-candidate-transformer
      :initform #'helm-magit:log-transform-candidates)
     (action
      :initform #'magit-show-commit)
     (persistent-action
      :initform #'magit-show-commit)))

  (defclass helm-magit:source-local-branches (helm-magit:source-log)
    ((init
      :initform #'helm-magit:local-branches-init)))

  (defclass helm-magit:source-remote-branches (helm-magit:source-log)
    ((init
      :initform #'helm-magit:remote-branches-init)))

  (defclass helm-magit:source-tags (helm-magit:source-log)
    ((init
      :initform #'helm-magit:tags-init)))

  (defclass helm-magit:source-head-commits (helm-magit:source-log)
    ((init
      :initform #'helm-magit:head-commits-init)))

  (defun helm-magit:revs (buffer action)
    (helm :sources
          (list (helm-make-source "Local branches"
                    'helm-magit:source-local-branches
                  :action action)
                (helm-make-source "Remote branches"
                    'helm-magit:source-remote-branches
                  :action action)
                (helm-make-source "Tags"
                    'helm-magit:source-tags
                  :action action)
                (helm-make-source "Commits in HEAD"
                    'helm-magit:source-head-commits
                  :action action))
          :buffer "*helm magit diff*"))

  (defun helm-magit:checkout ()
    (interactive)
    (helm-magit:revs
     "*helm magit checkout*"
     #'(lambda (commit)
         (magit-checkout (helm-magit:commit-shortname commit)))))

  (defun helm-magit:diff ()
    (interactive)
    (helm-magit:revs
     "*helm magit diff*"
     #'(lambda (commit)
         (magit-diff (helm-magit:commit-shortname commit))))))
