(setq viper-fast-keyseq-timeout 0)
(setq viper-expert-level 3)
(setq viper-case-fold-search t)
(setq viper-inhibit-startup-message t)
(setq viper-u-always-undo t)

(define-key viper-vi-global-user-map
  (kbd "j") 'viper-backward-char)
(define-key viper-vi-global-user-map
  (kbd "h") 'next-line)
(define-key viper-vi-global-user-map
  (kbd "t") 'previous-line)
(define-key viper-vi-global-user-map
  (kbd "k") 'viper-forward-char)
(define-key viper-vi-global-user-map
  (kbd "H") 'viper-scroll-up)
(define-key viper-vi-global-user-map
  (kbd "T") 'viper-scroll-down)

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
(define-key viper-dired-modifier-map "h" 'dired-next-line)
(define-key viper-dired-modifier-map "t" 'dired-previous-line)
(define-key viper-dired-modifier-map "/" 'dired-goto-file)
(define-key viper-dired-modifier-map "j"
  '(lambda () (interactive) (dired-next-line 10)))
(define-key viper-dired-modifier-map "k"
  '(lambda () (interactive) (dired-previous-line 10)))
