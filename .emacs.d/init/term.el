(require 'term+)
(require 'xterm-256color)
(require 'key-intercept)
(require 'multi-mode-util)
(require 'term+mux)

;;; session colors

(defvar term+mux-session-user-color-alist
  '(("root" . "#fc391f")))
(defvar term+mux-session-host-color-alist '())
(defvar term+mux-session-local-color "#8c8ce8")
(defvar term+mux-session-remote-color "DarkKhaki")

(defun term+mux-session-color (session)
  (when session
    (let* ((user (term+mux-session-user session))
           (host (term+mux-session-host session))
           (user-color (cdr (assoc user term+mux-session-user-color-alist)))
           (host-color (cdr (assoc host term+mux-session-host-color-alist)))
           (host (term+shorten-hostname host))
           (hst-color (cdr (assoc host term+mux-session-host-color-alist))))
      (or user-color host-color hst-color
          (if (term+mux-session-local-p session)
              term+mux-session-local-color
            term+mux-session-remote-color)))))

(defun term+mux-colored-group-label (group)
  (let* ((label (tab-group:group-button-label group))
         (color (tab-group:get 'group-color group))
         (face `(:inherit tab-group:group :foreground ,color)))
    (propertize label 'face face)))

(defun term+mux-set-group-color-function (session own-group)
  (when own-group
    (let ((group (term+mux-session-group session))
          (color (term+mux-session-color session)))
      (tab-group:set 'group-color color group)
      (tab-group:set 'group-label 'term+mux-colored-group-label group))))

(add-hook 'term+mux-new-session-hook 'term+mux-set-group-color-function)
