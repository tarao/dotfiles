;; tabbing
(setq-default tab-width 4 indent-tabs-mode nil)

;; align
(require 'align nil t)

;; auto-insert
(when (require 'autoinsert nil t)
  (add-hook 'find-file-not-found-hooks 'auto-insert)
  (setq auto-insert-directory "~/.emacs.d/insert/")
  (setq auto-insert-query nil)
  (setq auto-insert-alist nil))

;; automatically make script executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; browse-kill-ring
(bundle browse-kill-ring)
(bundle browse-kill-ring+
  (define-key global-map (kbd "M-y") 'browse-kill-ring)
  (setq kill-do-not-save-duplicates t))

;; undo/redo
(bundle undo-tree
  (global-undo-tree-mode)
  (setq undo-tree-mode-lighter nil))

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
