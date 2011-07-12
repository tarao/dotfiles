(require 'color-moccur)
(global-set-key (kbd "C-M-m") 'moccur-grep-find)

(defun moccur-grep-find-region (beg end &optional dir)
  (interactive "r\nDDirectory: ")
  (unless dir (setq dir (file-name-directory (buffer-file-name))))
    (moccur-grep-find dir (list (buffer-substring-no-properties beg end))))
