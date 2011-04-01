(defun mew-summary-display-and-select-window ()
  (interactive)
  (mew-summary-msg-or-part
   (let* ((win (selected-window))
          (msg (mew-summary-message-number))
          (part (mew-syntax-nums))
          (fid (mew-frame-id))
          (omsg (mew-current-get-msg fid))
          (opart (mew-current-get-part fid)))
     (unless (or (and msg (string= msg omsg) (null part) (null opart))
             (and part (equal part opart)))
       (call-interactively 'mew-summary-display)) ; ensure displaying message
     (mew-summary-toggle-disp-msg 'on)
     (mew-window-configure 'message))))

(defun mew-message-close ()
  (interactive)
  (mew-message-goto-summary)
  (mew-summary-toggle-disp-msg))

(defun mew-summary-refile-spam ()
  (interactive)
  (mew-summary-msg-or-part
   (mew-summary-not-in-draft
    (mew-summary-local-or-imap
     (mew-summary-refile-body '("+spam"))))))
;; patch
(defadvice mew-draft-mode (before major-mode-convention activate)
  (kill-all-local-variables))

(defun mew-install-user-map ()
  ;; mew-summary-mode key maps
  (define-key mew-summary-mode-map (kbd "RET")
    'mew-summary-display-and-select-window)
  (define-key mew-summary-mode-map (kbd "SPC") 'mew-summary-scroll-up)
  (define-key mew-summary-mode-map (kbd "C-@") 'mew-summary-scroll-down)
  (define-key mew-summary-mode-map (kbd "C-SPC") 'mew-summary-scroll-down)
  (define-key mew-summary-mode-map "s" 'mew-summary-refile-spam)
  (define-key mew-summary-mode-map (kbd "l s") 'mew-summary-ls)
  (when (featurep 'vimpulse)
    (vimpulse-add-vi-bindings mew-summary-mode-map
                              '(viper-next-line
                                viper-previous-line
                                viper-scroll-down
                                viper-scroll-up
                                viper-goto-line
                                anything-for-files) t))
  ;; mew-message-mode key maps
  (when (featurep 'vimpulse)
    (vimpulse-add-core-movement-cmds mew-message-mode-map)
    (vimpulse-add-movement-cmds mew-message-mode-map t)
  (vimpulse-add-vi-bindings mew-message-mode-map '(anything-for-files) t))
  (define-key mew-message-mode-map (kbd "q") 'mew-message-close))
(eval-after-load 'mew-key '(mew-install-user-map))

(add-hook
 'mew-summary-mode-hook
 '(lambda () (set (make-local-variable 'show-trailing-whitespace) nil)))

;; mew-start-process-disp sets DISPLAY from frame-parameters
;; We know that DISPLAY has appropriate value
;; and no need to lookup frame-parameters
(defadvice mew-start-process-disp (before ad-mew-start-process-disp activate)
  (set-frame-parameter nil 'display (getenv "DISPLAY")))

;; no end mark for mew-summary-mode
(when (featurep 'end-mark)
  (add-to-list 'end-mark-exclude-modes 'mew-summary-mode))
