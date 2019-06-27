(defvar compilation-original-buffer nil)

(defadvice compilation-start (after save-original-buffer activate)
  (let ((buffer (current-buffer)))
    (with-current-buffer next-error-last-buffer
      (set (make-local-variable 'compilation-original-buffer) buffer))))

(defun compilation/colorize ()
  (require 'compile)
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'compilation/colorize)
