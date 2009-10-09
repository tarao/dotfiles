(defun join-to-string (s list) (mapconcat 'identity list s))

(defun my-template ()
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (replace-string (car c) (funcall (cdr c)) nil)))
        template-replacement-alist)
  (goto-char (point-max)))
(setq template-replacement-alist nil)
