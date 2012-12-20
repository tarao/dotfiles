;; make *scratch* immortal
(bundle immortal-buffer :name tarao-elisp
  (make-buffer-immortal "*scratch*"))

;; auto-save
(defun auto-save-buffer (&optional buffer)
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (if (and buffer-file-name
         (buffer-modified-p)
             (not buffer-read-only)
             (file-writable-p buffer-file-name))
        (save-buffer))))

;; no backup
(defun no-backup ()
  (interactive)
  (set (make-variable-buffer-local 'make-backup-files) nil)
  (auto-save-mode 0))

;; use directory name instead of <num>
(when (require 'uniquify nil 'noerror)
  (setq uniquify-buffer-name-style 'forward))

;; Buffer-menu
(add-hook 'Buffer-menu-mode-hook
          #'(lambda ()
              (hl-line-mode 1)
              (setq show-trailing-whitespace nil)))
