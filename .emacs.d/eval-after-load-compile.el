(eval '(eval-when-compile (require 'cl)))

(defmacro eval-after-load-compile (feature &rest form)
  (declare (indent defun))
  (let ((feat (if (and (listp feature) (eq (nth 0 feature) 'quote))
                  (nth 1 feature) feature)))
    (eval '(eval-when (compile)
             (cond ((stringp feat) (load feat t))
                   ((symbolp feat) (require feat nil t)))))
    (if (cond ((stringp feat) (locate-library feat))
              ((symbolp feat) (featurep feat)))
        ;; byte-compiled version
        `(eval-after-load ,feature
           '(funcall ,(byte-compile `(lambda () ,@form))))
      ;; normal version
      `(eval-after-load ,feature '(progn ,@form)))))

(provide 'eval-after-load-compile)
