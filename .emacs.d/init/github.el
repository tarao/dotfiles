(eval-when-compile (require 'cl))

(defun open-github--command-to-string (args)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'vc-git-command (current-buffer) t nil args))))

(defun open-github--command-one-line (&rest args)
  (let* ((output (open-github--command-to-string args))
         (output (replace-regexp-in-string "[\r\n]+$" "" output)))
    (and (> (length output) 0) output)))

(defun open-github--command-lines (&rest args)
  (let* ((output (open-github--command-to-string args)))
    (split-string (open-github--command-to-string args) "[\r\n]+")))

(defun open-github--root ()
  (let ((root (open-github--command-one-line "rev-parse" "--show-toplevel")))
    (unless root
      (error "Error: here is not Git repository"))
    (file-name-as-directory root)))

(defun open-github--host (&optional remote-url)
  (or (open-github--command-one-line "config" "--get" "hub.host")
      (and remote-url
           (string-match "^[^:]+://\\(?:[^/]+@\\)?\\([^/]+\\)/" remote-url)
           (match-string 1 remote-url))
      "github.com"))

(defun open-github--current-branch ()
  (open-github--command-one-line "symbolic-ref" "--short" "HEAD"))

(defun open-github--remote (&optional branch)
  (let ((branch (or branch (open-github--current-branch))))
    (open-github--command-one-line
     "config" (format "branch.%s.remote" branch))))

(defun open-github--remote-url (remote)
  (or (open-github--command-one-line "remote" "get-url" remote)
      (error "Failed to get %s" remote)))

(defun open-github--find-remote-branch-from-sha1 (sha1)
  (let ((branches (open-github--command-lines "branch" "--contains" sha1)))
    (loop for branch in branches
          when (> (length branch) 2)
          for branch = (substring branch 2)
          for remote = (open-github--remote branch)
          when (not (null remote))
          return (cons remote branch))))

(defun open-github--find-remote-branch-from-commits (commits)
  (loop for sha1 in commits
        for r = (open-github--find-remote-branch-from-sha1 sha1)
        when (not (null r))
        return r))

(defun open-github--find-remote-branch ()
  (let* ((branch (open-github--current-branch))
         (remote (open-github--remote branch)))
    (if remote (cons remote branch)
      (open-github--find-remote-branch-from-commits
       (open-github--command-lines "log" "--pretty=format:%H")))))

(defun open-github--sha1 (&optional remote branch)
  (let* ((branch (or branch (open-github--current-branch)))
         (remote (or remote (open-github--remote branch)))
         (remote-branch (format "%s/%s" remote branch)))
    (unless remote-branch
      (error "Failed to get remote SHA1"))
    (open-github--command-one-line "rev-parse" remote-branch)))

(defun open-github--highlight-marker (start end)
  (cond ((and start end (< start end)) (format "#L%s..L%s" start end))
        (start (format "#L%s" start))
        (t "")))

(defun open-github--extract-user-repo (remote-url)
  (if (string-match "[:/]\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?\\'" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url))
    (error "Failed: match %s" remote-url)))

(defun open-github--file-url (host remote sha1 file marker)
  (let ((user-repo (open-github--extract-user-repo remote)))
    (format "https://%s/%s/%s/blob/%s/%s%s"
            host (car user-repo) (cdr user-repo) sha1 file marker)))

(defun open-github--from-file (file &optional start end)
  (let* ((remote-branch (or (open-github--find-remote-branch)
                            (error "Failed to find remote branch")))
         (remote (car remote-branch))
         (branch (cdr remote-branch))
         (remote-url (open-github--remote-url remote))
         (host (open-github--host remote-url))
         (sha1 (open-github--sha1 remote branch))
         (marker (open-github--highlight-marker start end)))
    (browse-url (open-github--file-url host remote-url sha1 file marker))))

(defun open-github--from-file-direct (file start end)
  (let* ((file (file-truename (expand-file-name file)))
         (default-directory (file-name-directory file))
         (root (open-github--root))
         (path (file-relative-name file root))
         (start-line (and start (line-number-at-pos start)))
         (end-line (and end (1- (line-number-at-pos end)))))
    (open-github--from-file path start-line end-line)))

(defun open-github-from-file ()
  (interactive)
  (let ((start (and mark-active (region-beginning)))
        (end (and mark-active (region-end))))
    (open-github--from-file-direct (buffer-file-name) start end)))
