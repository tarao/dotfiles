;; use darker comment
(defun set-comment-color (color)
  (set-face-foreground 'font-lock-comment-delimiter-face color)
  (set-face-foreground 'font-lock-comment-face color))

(defun darken-comment ()
  (interactive)
  (set-comment-color "gray32"))

(defun lighten-comment ()
  (interactive)
  (set-comment-color "OrangeRed"))

(darken-comment)
