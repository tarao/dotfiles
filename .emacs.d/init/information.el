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
  (global-git-gutter-mode t))

;; git-messenger
(bundle! git-messenger)

;; eldoc
(setq-default eldoc-idle-delay 0
              eldoc-echo-area-use-multiline-p t)
(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                ielm-mode-hook))
  (add-hook hook #'turn-on-eldoc-mode))
(bundle c-eldoc
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook #'(lambda ()
                       (set (make-local-variable 'eldoc-idle-delay) 0.3)
                       (c-turn-on-eldoc-mode)))))
(bundle eldoc-extension)
