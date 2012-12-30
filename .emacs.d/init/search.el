(unless (featurep 'ibuffer) ; patch
  (defun ibuffer-unmark-all (mark)))
(bundle color-moccur
  (setq-default moccur-grep-default-word-near-point t)
  (global-set-key (kbd "C-M-m") #'moccur-grep-find)

  (autoload 'moccur-grep-read-directory "color-moccur")
  (defun moccur-grep-find-region (beg end &optional dir)
    (interactive "r")
    (unless dir (setq dir (moccur-grep-read-directory)))
    (moccur-grep-find dir (list (buffer-substring-no-properties beg end)))))
