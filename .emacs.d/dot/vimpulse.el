;; viper minor mode
(setq viper-custom-file-name (locate-library "dot/.viper"))
(setq viper-mode t)
(setq viper-toggle-key (kbd "C-x C-z"))
(setq viper-ex-style-motion nil)
(setq viper-ex-style-editing nil)
(setq blink-matching-paren t)
(require 'viper)
(setq viper-vi-state-id "")
(setq viper-insert-state-id "INSERT")
(setq viper-visual-characterwise-state-id "VISUAL")
(setq viper-visual-linewise-state-id "VLINE")
(setq viper-visual-blockwise-state-id "VBLOCK")
(setq viper-emacs-state-id "x")

;; viper-mode patches
(require 'hexl-viper-patch)
(defadvice viper-maybe-checkout (around viper-dont-ask-checkout activate) nil)

(setq vimpulse-want-vi-keys-in-dired nil)
(setq woman-use-own-frame nil) ; don't create new frame for manpages
(require 'vimpulse)

;; viper-mode keymaps
(define-key vimpulse-visual-basic-map
  (kbd ";") (lambda () (interactive) (viper-ex t)))
(define-key viper-vi-basic-map
  (kbd ";") 'viper-ex)
(define-key viper-vi-global-user-map
  (kbd ":") 'anything)
(define-key viper-vi-basic-map
  (kbd "TAB") nil)
(define-key viper-vi-global-user-map
  (kbd "C-w") 'kill-region)
(define-key viper-insert-global-user-map
  (kbd "C-w") 'kill-region)
(define-key viper-insert-basic-map
  (kbd "C-p") nil)
(define-key viper-insert-basic-map
  (kbd "C-n") nil)

;; vimpulse patches
