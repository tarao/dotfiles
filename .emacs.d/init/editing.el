;; tabbing
(setq-default tab-width 4
              indent-tabs-mode nil)

;; align
(require 'align nil t)

;; auto-insert
(require 'autoinsert nil t)
(add-hook 'find-file-not-found-hooks #'auto-insert)
(setq auto-insert-directory "~/.emacs.d/insert/"
      auto-insert-query nil
      auto-insert-alist nil)

;; automatically make script executable
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; undo/redo
(bundle undo-tree
  (setq-default undo-tree-mode-lighter nil)
  (global-undo-tree-mode))

;; path function
(defun backward-kill-path-element ()
  (interactive)
  (let ((pt (point)))
    (when (not (bolp))
      (backward-char)
      (re-search-backward "/" nil t)
      (forward-char)
      (when (= (point) pt) (call-interactively 'move-beginning-of-line))
      (kill-region (point) pt))))
