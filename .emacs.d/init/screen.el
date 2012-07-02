(defvar screen-import-env '(DISPLAY))

(defun screen-getenv (sty name)
  (let ((coding-system-for-read 'no-conversion)
        (coding-system-for-write 'no-conversion)
        (cmd (concat "screen -S '" sty "' -Q echo '$" name "'")))
    (with-temp-buffer
      (when (= 0 (shell-command cmd (current-buffer)))
        (buffer-string)))))

(defun screen-sync-env (sty)
  (interactive (list (getenv "STY")))
  (dolist (sym screen-import-env)
    (let* ((name (symbol-name sym))
           (env (screen-getenv sty name)))
      (setenv name (or env "")))))
