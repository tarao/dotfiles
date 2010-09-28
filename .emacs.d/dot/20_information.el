;; show line numbers
(require 'linum)
(require 'linum+)
(global-set-key (kbd "M-n") 'linum-mode)
(global-set-key (kbd "M-N") 'relative-linum-mode)
(set-face-attribute 'linum nil :foreground "aquamarine4")

;; wc (CC/WW/LL)
(autoload 'word-count-mode "word-count" "Minor mode to count words." t nil)
(global-set-key (kbd "M-+") 'word-count-mode)

;; eldoc
(require 'c-eldoc)
(require 'eldoc-extension)
(setq eldoc-idle-delay 0)
(setq eldoc-echo-area-use-multiline-p t)
(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                ielm-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook '(lambda ()
                    (set (make-local-variable 'eldoc-idle-delay) 0.3)
                    (c-turn-on-eldoc-mode))))
