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

(defun compilation/maximize-test-window (&rest args)
  (when (string-match "\\btest\\b" (buffer-name))
    (message "buffer: %s" (buffer-name))
    (with-current-buffer (buffer-name)
      (delete-other-windows (get-buffer-window (buffer-name))))))

(defun compilation/has-message ()
  (compilation-next-single-property-change (point-min) 'compilation-message))

(defun compilation/hide-successful-compilation-result (buf msg)
  (with-current-buffer buf
    (let ((window (get-buffer-window buf)))
      (if (or (not (string-match "^finished\\b" msg))
              (string-match "\\btest\\b" (buffer-name buf))
              (compilation/has-message))
          (when window
            (select-window window))
        (when window
          (delete-window window))
        (message "Compilation finished successfully")))))

(add-hook 'compilation-filter-hook #'compilation/colorize)
(add-hook 'compilation-start-hook #'compilation/maximize-test-window)
(add-hook 'compilation-finish-functions
          #'compilation/hide-successful-compilation-result)
