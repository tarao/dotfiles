(setq-default magit-diff-refine-hunk 'all
              magit-display-buffer-function
              #'(lambda (buffer)
                  (display-buffer buffer '(display-buffer-same-window)))
              magit-auto-revert-mode nil)
(bundle magit
  (with-eval-after-load-feature 'magit-diff
    (let ((map magit-diff-mode-map))
      (define-key map (kbd "RET") 'magit-ediff-dwim)
      (define-key map "v" 'magit-diff-visit-file)
      (define-key map (kbd "M-.") 'magit-diff-visit-file)
      (define-key map (kbd "SPC") 'magit-jump-to-diffstat-or-diff)))
  (with-eval-after-load-feature 'magit-section
    (add-hook 'magit-section-movement-hook
              'magit-status-maybe-update-blob-buffer)))

(bundle git-messenger
  (defmacro with-closing-git-messenger-popup (fall-back &rest body)
    (declare (indent 1) (debug t))
    `(if (not (eq git-messenger:vcs 'git))
         (call-interactively #',fall-back)
       ,@body
       (git-messenger:popup-close)))

  (defun git-messenger-inherit-face-foreground (str face)
    (propertize str 'face
                `(:inherit popup-tip-face
                           :weight normal
                           :foreground ,(face-foreground face))))

  (defun git-messenger:format-detail-with-highlight (orig-fun vcs commit-id author message)
    (eval-and-compile (require 'git-messenger))
    (eval-and-compile (require 'magit))
    (if (not (eq vcs 'git))
        (funcall orig-fun vcs commit-id author message)
      (let ((date (git-messenger:commit-date commit-id)))
        (format "%s \nAuthor : %s\nDate   : %s \n%s"
                (git-messenger-inherit-face-foreground
                 (substring commit-id 0 8) 'magit-hash)
                (git-messenger-inherit-face-foreground author 'magit-popup-key)
                (git-messenger-inherit-face-foreground date 'magit-log-date)
                message))))
  (advice-add 'git-messenger:format-detail
              :around #'git-messenger:format-detail-with-highlight)

  (defun git-messenger:commit-date-iso (orig-fun commit-id)
    (eval-and-compile (require 'git-messenger))
    (let ((args `("--no-pager" "show" "--pretty=%ad" "--date=iso" ,commit-id)))
      (with-temp-buffer
        (unless (zerop (git-messenger:execute-command 'git args t))
          (error "Failed 'git show'"))
        (goto-char (point-min))
        (buffer-substring-no-properties
         (line-beginning-position) (line-end-position)))))
  (advice-add 'git-messenger:commit-date
              :around #'git-messenger:commit-date-iso)

  (with-eval-after-load-feature 'git-messenger
    (let ((map git-messenger-map))
      (define-key map (kbd "d")
        #'(lambda ()
            (interactive)
            (with-closing-git-messenger-popup git-messenger:popup-diff
              (magit-diff (concat git-messenger:last-commit-id "^!")))))
      (define-key map (kbd "s")
        #'(lambda ()
            (interactive)
            (with-closing-git-messenger-popup git-messenger:popup-show
              (magit-show-commit git-messenger:last-commit-id)))))))
