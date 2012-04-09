(require 'color-moccur)
(global-set-key (kbd "C-M-m") 'moccur-grep-find)
(setq moccur-grep-default-word-near-point t)

(defun moccur-grep-find-region (beg end &optional dir)
  (interactive "r")
  (unless dir (setq dir (moccur-grep-read-directory)))
  (moccur-grep-find dir (list (buffer-substring-no-properties beg end))))
