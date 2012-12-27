(defvar template-replacement-alist nil)

(defun template-replacer ()
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (perform-replace (car c) (funcall (cdr c)) nil nil nil)))
        template-replacement-alist)
  (goto-char (point-max)))
