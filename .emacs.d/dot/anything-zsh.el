;; See: http://d.hatena.ne.jp/rubikitch/20091208/anythingzsh
(require 'anything-complete)

(defun anything-zsh-history-from-zle ()
  (interactive)
  (let ((anything-samewindow t)
        (anything-display-function 'anything-default-display-buffer))
    (azh/set-command
     (anything-other-buffer
      `(((name . "History")
         (action
          ("Paste" . identity)
          ("Edit" . azh/edit-command))
         ,@anything-c-source-complete-shell-history))
      "*anything zsh history*"))))

(defvar azh/tmp-file "~/.zsh/.azh-tmp-file")
(defvar azh/frame nil)

(defun azh/set-command (line)
  (write-region (or line "") nil azh/tmp-file)
  (delete-frame))

(defun azh/edit-command (line)
  (switch-to-buffer "*zsh command edit*")
  (erase-buffer)
  (setq buffer-undo-list nil)
  (azh/edit-mode)
  (insert line)
  (recursive-edit)
  (buffer-string))

(define-derived-mode azh/edit-mode fundamental-mode
  "Press C-c C-c to exit!"
  "Edit zsh command line"
  (define-key azh/edit-mode-map "\C-c\C-c" 'azh/edit-exit))

(defun azh/edit-exit ()
  (interactive)
  (exit-recursive-edit))
