(defun open-github--command-to-string (args)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'vc-git-command (current-buffer) t nil args))))

(defun open-github--command-one-line (&rest args)
  (let* ((output (open-github--command-to-string args))
         (output (replace-regexp-in-string "[\r\n]+$" "" output)))
    (and (> (length output) 0) output)))

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

(defun open-github--remote-url ()
  (or (open-github--command-one-line "config" "--get" "remote.origin.url")
      (error "Failed to get remote.origin URL")))

(defun open-github--sha1 ()
  (let* ((branch (or (open-github--command-one-line "symbolic-ref" "HEAD") ""))
         (branch (replace-regexp-in-string "\\`refs/heads/" "" branch))
         (remote (open-github--command-one-line
                  "config" "--get" (format "branch.%s.remote" branch)))
         (remote-branch (format "%s/%s" remote branch)))
    (unless remote
      (error "Failed to get remote SHA1"))
    (open-github--command-one-line "rev-parse" remote-branch)))

(defun open-github--highlight-marker (start end)
  (cond ((and start end (< start end)) (format "#L%s..L%s" start end))
        (start (format "#L%s" start))
        (t "")))

(defun open-github--extract-user-host (remote-url)
  (if (string-match "[:/]\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?\\'" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url))
    (error "Failed: match %s" remote-url)))

(defun open-github--file-url (host remote sha1 file marker)
  (let ((user-repo (open-github--extract-user-host remote)))
    (format "https://%s/%s/%s/blob/%s/%s%s"
            host (car user-repo) (cdr user-repo) sha1 file marker)))

(defun open-github--from-file (file &optional start end)
  (let* ((remote-url (open-github--remote-url))
         (host (open-github--host remote-url))
         (sha1 (open-github--sha1))
         (marker (open-github--highlight-marker start end)))
    (browse-url (open-github--file-url host remote-url sha1 file marker))))

(defun open-github--from-file-direct (file start end)
  (let* ((root (open-github--root))
         (file (file-truename (expand-file-name file)))
         (path (file-relative-name file root))
         (start-line (and start (line-number-at-pos start)))
         (end-line (and end (1- (line-number-at-pos end)))))
    (open-github--from-file path start-line end-line)))

(defun open-github-from-file ()
  (interactive)
  (let ((start (and mark-active (region-beginning)))
        (end (and mark-active (region-end))))
    (open-github--from-file-direct (buffer-file-name) start end)))
