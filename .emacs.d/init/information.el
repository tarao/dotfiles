;; show line numbers
(when (require 'linum nil t)
  (global-set-key (kbd "M-n") #'linum-mode)
  (set-face-attribute 'linum nil :foreground "aquamarine4"))
(bundle tarao-elisp
  (global-set-key (kbd "M-N") #'relative-linum-mode))

;; wc (CC/WW/LL)
(defun add-local-hook (hook function &optional append)
  (add-hook hook function append t))
(defun remove-local-hook (hook function)
  (remove-hook hook function t))
(bundle word-count
  (global-set-key (kbd "M-+") #'word-count-mode))

;; git-gutter
(bundle! git-gutter-fringe
  (global-git-gutter-mode t)
  (setq git-gutter:lighter " GG"))

;; git-messenger
(bundle noflet)
(bundle git-messenger
  (setq-default git-messenger:show-detail t)

  (defun git-messenger:replace-popup-tip (orig-fun &rest args)
    (eval-and-compile (require 'noflet))
    (let ((popup-tip (symbol-function 'popup-tip)))
      (noflet ((popup-tip (&rest args)
                 (apply popup-tip (append args '(:margin 1 :nostrip t)))))
        (apply orig-fun args))))
  (advice-add 'git-messenger:popup-message
              :around #'git-messenger:replace-popup-tip))

;; eldoc
(setq-default eldoc-idle-delay 0.1
              eldoc-echo-area-use-multiline-p t
              flycheck-display-errors-delay 0.2)
(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                ielm-mode-hook))
  (add-hook hook #'eldoc-mode))
(bundle c-eldoc
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook #'(lambda ()
                       (set (make-local-variable 'eldoc-idle-delay) 0.3)
                       (c-turn-on-eldoc-mode)))))
(bundle eldoc-extension)
