(eval-when-compile
  (unless (require 'mew nil t)
    ;; suppress compiler warnings
    (defvar mew-summary-mode-map nil)
    (defvar mew-message-mode-map nil)
    (autoload 'mew-syntax-nums "mew")
    (autoload 'mew-frame-id "mew")
    (autoload 'mew-current-get-msg "mew")
    (autoload 'mew-current-get-part "mew")
    (autoload 'mew-window-configure "mew")
    (autoload 'mew-summary-msg-or-part "mew")
    (autoload 'mew-summary-message-number "mew")
    (autoload 'mew-summary-not-in-draft "mew")
    (autoload 'mew-summary-local-or-imap "mew")
    (autoload 'mew-summary-refile-body "mew")
    (autoload 'mew-summary-toggle-disp-msg "mew" nil t)
    (autoload 'mew-message-goto-summary "mew" nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands and keys

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

(defun mew-install-user-map ()
  ;; mew-summary-mode key maps
  (define-key mew-summary-mode-map (kbd "RET")
    #'mew-summary-display-and-select-window)
  (define-key mew-summary-mode-map (kbd "SPC") #'mew-summary-scroll-up)
  (define-key mew-summary-mode-map (kbd "C-@") #'mew-summary-scroll-down)
  (define-key mew-summary-mode-map (kbd "C-SPC") #'mew-summary-scroll-down)
  (define-key mew-summary-mode-map "s" #'mew-summary-refile-spam)
  (define-key mew-summary-mode-map (kbd "l s") #'mew-summary-ls)

  ;; mew-message-mode key maps
  (define-key mew-message-mode-map (kbd "q") #'mew-message-close))
(eval-after-load 'mew-key '(mew-install-user-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; patches

;; summary fix
(add-hook
 'mew-summary-mode-hook
 #'(lambda () (set (make-local-variable 'show-trailing-whitespace) nil)))
(eval-after-load 'end-mark
  '(add-to-list 'end-mark-exclude-modes 'mew-summary-mode))

(defadvice mew-draft-mode (before major-mode-convention activate)
  (kill-all-local-variables))

(defadvice mew-start-process-disp (before ad-mew-start-process-disp activate)
  "`mew-start-process-disp' sets DISPLAY from frame parameters.
We know that DISPLAY has appropriate value and no need to lookup
frame parameters."
  (set-frame-parameter nil 'display (getenv "DISPLAY")))
