(defun install-packages (&optional no-fetch)
  (when (require 'package)
    ;; package source
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (package-initialize)
    (unless no-fetch
      ;; Read the locally-cached archive-contents.
      (package-read-all-archive-contents)
      ;; Fetch the remote list of packages.
      (package-refresh-contents))

    ;; install
    (let* ((package-list-file
           (expand-file-name "~/.emacs.d/elpa/package-list")))
      (when (file-exists-p package-list-file)
        (with-temp-buffer
          (insert-file-contents package-list-file)
          (goto-char (point-min))
          (while (not (eolp))
            (let* ((line (buffer-substring (point) (line-end-position)))
                   (package (and (> (length line) 0) (intern line))))
              (unless (package-installed-p package)
                (package-install package)))
            (forward-line)))))))
