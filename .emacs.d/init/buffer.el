;; make *scratch* immortal
(bundle tarao-elisp
  (make-buffer-immortal "*scratch*"))

;; auto-save
(defun auto-save-buffer (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (when (and buffer-file-name
             (buffer-modified-p)
             (not buffer-read-only)
             (file-writable-p buffer-file-name))
      (save-buffer))))

;; no backup
(defun no-backup ()
  (interactive)
  (set (make-local-variable 'make-backup-files) nil)
  (auto-save-mode 0))

;; use directory name instead of <num>
(require 'uniquify nil 'noerror)
(setq uniquify-buffer-name-style 'forward)

;; Buffer-menu
(add-hook 'Buffer-menu-mode-hook
          #'(lambda ()
              (hl-line-mode 1)
              (setq show-trailing-whitespace nil)))
