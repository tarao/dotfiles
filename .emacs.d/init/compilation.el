(defvar compilation-original-buffer nil)

(defadvice compilation-start (after save-original-buffer activate)
  (let ((buffer (current-buffer)))
    (with-current-buffer next-error-last-buffer
      (set (make-local-variable 'compilation-original-buffer) buffer))))
