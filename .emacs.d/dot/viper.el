(setq viper-fast-keyseq-timeout 0)
(setq viper-expert-level 3)
(setq viper-case-fold-search t)
(setq viper-inhibit-startup-message t)
(setq viper-u-always-undo t)

(define-key viper-vi-global-user-map
  (kbd "j") 'next-line)
(define-key viper-vi-global-user-map
  (kbd "k") 'previous-line)
(define-key viper-vi-global-user-map
  (kbd "J") 'viper-scroll-up)
(define-key viper-vi-global-user-map
  (kbd "K") 'viper-scroll-down)

(defun my-viper-jump-tag ()
  (interactive)
  (setq wd (thing-at-point 'symbol))
  (find-tag wd))
(define-key viper-vi-global-user-map (kbd "C-]") 'my-viper-jump-tag)

(defun my-viper-jump-tag-next ()
  (interactive)
  (setq wd (thing-at-point 'symbol))
  (find-tag wd 0))
(define-key viper-vi-global-user-map (kbd "C-:") 'my-viper-jump-tag-next)

(defun my-viper-pop-tag ()
  (interactive)
  (pop-tag-mark))
(define-key viper-vi-global-user-map (kbd "C-t") 'my-viper-pop-tag)

(defun my-viper-pop-mark ()
  (interactive)
  (set-mark-command -1))
(define-key viper-vi-global-user-map (kbd "C-o") 'my-viper-pop-mark)

;; dired
(define-key viper-dired-modifier-map "j" 'dired-next-line)
(define-key viper-dired-modifier-map "k" 'dired-previous-line)
(define-key viper-dired-modifier-map "/" 'dired-goto-file)
(define-key viper-dired-modifier-map "l"
  '(lambda () (interactive) (dired-next-line 10)))
(define-key viper-dired-modifier-map "h"
  '(lambda () (interactive) (dired-previous-line 10)))
