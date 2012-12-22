(defvar template-replacement-alist nil)

(defun template-replacer ()
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (replace-string (car c) (funcall (cdr c)) nil)))
        template-replacement-alist)
  (goto-char (point-max)))
