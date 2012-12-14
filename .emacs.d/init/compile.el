(defun batch-byte-compile-with-package ()
  (when (require 'package nil t)
    (package-initialize))
  (batch-byte-compile))
