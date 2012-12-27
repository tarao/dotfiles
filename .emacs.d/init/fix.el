(defvar input-decode-alist-xterm '(("\e[1;2A" . [S-up])))

(defun install-input-decode-alist (alist)
  (dolist (elt alist)
    (define-key input-decode-map (car elt) (cdr elt))))

(when (equal "xterm" (tty-type))
  (install-input-decode-alist input-decode-alist-xterm))
(defadvice terminal-init-xterm (after ad-install-decode-xterm activate)
  (install-input-decode-alist input-decode-alist-xterm))
