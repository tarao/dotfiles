;; See: http://d.hatena.ne.jp/rubikitch/20091208/anythingzsh
(require 'anything-complete)

(defun anything-zsh-history-from-zle (file &optional input)
  (interactive)
  (let ((anything-samewindow t)
        (anything-display-function 'anything-default-display-buffer))
    (azh/set-command
     (anything
      `(((name . "History")
         (action
          ("Paste" . identity)
          ("Edit" . azh/edit-command))
         ,@anything-c-source-complete-shell-history))
      input
      nil nil nil
      "*anything zsh history*")
     file)))

(defun azh/set-command (line file)
  (write-region (or line "") nil file)
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
