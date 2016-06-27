(setq-default ediff-window-setup-function 'ediff-setup-windows-plain
              ediff-keep-variants nil
              ediff-sbs:wide-display-grows-to 'left)

(with-eval-after-load-feature 'winner
  (add-hook 'ediff-quit-hook 'winner-undo))

(with-eval-after-load-feature 'ediff-init
  (set-face-foreground 'ediff-current-diff-A nil)
  (set-face-foreground 'ediff-current-diff-B nil)
  (set-face-foreground 'ediff-current-diff-C nil)
  (set-face-foreground 'ediff-current-diff-Ancestor nil)
  (set-face-foreground 'ediff-fine-diff-A nil)
  (set-face-foreground 'ediff-fine-diff-B nil)
  (set-face-foreground 'ediff-fine-diff-C nil)
  (set-face-foreground 'ediff-fine-diff-Ancestor nil))

(with-eval-after-load-feature 'ediff-util
  ;; Do not ask when Ediff session quits
  (defun ediff-quit-without-asking (reverse-default-keep-variants)
    (interactive "P")
    (ediff-barf-if-not-control-buffer)
    (let ((ctl-buf (current-buffer))
          (ctl-frm (selected-frame))
          (minibuffer-auto-raise t))
      (message "")
      (set-buffer ctl-buf)
      (ediff-really-quit reverse-default-keep-variants)))
  (add-hook 'ediff-keymap-setup-hook
            #'(lambda ()
                (let ((map ediff-mode-map))
                  (define-key map "q" 'ediff-quit-without-asking))))
  (advice-add 'ediff-janitor :around
              #'(lambda (orig-fun ask keep-variants)
                  (funcall orig-fun nil keep-variants))))

(bundle ediff-side-by-side
  (with-eval-after-load-feature 'ediff
    (require 'ediff-side-by-side)))
