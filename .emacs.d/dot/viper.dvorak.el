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

;; dired
(define-key viper-dired-modifier-map "h" 'dired-next-line)
(define-key viper-dired-modifier-map "t" 'dired-previous-line)
(define-key viper-dired-modifier-map "j"
  '(lambda () (interactive) (dired-next-line 10)))
(define-key viper-dired-modifier-map "k"
  '(lambda () (interactive) (dired-previous-line 10)))
