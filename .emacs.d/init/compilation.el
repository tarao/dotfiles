(defvar original-buffer)

(defadvice compilation-start (after save-original-buffer activate)
  (let ((buffer (current-buffer)))
    (with-current-buffer next-error-last-buffer
      (set (make-local-variable 'original-buffer) buffer))))

(defun compilation-original-buffer ()
  original-buffer)
