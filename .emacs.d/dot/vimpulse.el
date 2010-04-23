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

;; auto-complete patches
(define-key ac-completing-map (kbd "ESC") nil)

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
(define-key viper-vi-basic-map
  (kbd "C-r") 'undo-tree-redo)

(defadvice undo-tree-visualize
  (after ad-undo-tree-visualize-remove-viper-keys activate)
  (make-variable-buffer-local 'viper-vi-basic-map)
  (define-key viper-vi-basic-map (kbd "b") nil)
  (define-key viper-vi-basic-map (kbd "f") nil)
  (define-key viper-vi-basic-map (kbd "n") nil)
  (define-key viper-vi-basic-map (kbd "p") nil)
  (define-key viper-vi-basic-map (kbd "q") nil)
  (define-key viper-vi-basic-map (kbd "t") nil)
  (define-key viper-vi-basic-map (kbd ",") nil)
  (define-key viper-vi-basic-map (kbd ".") nil)
  (define-key viper-vi-basic-map (kbd "<") nil)
  (define-key viper-vi-basic-map (kbd ">") nil)
  (define-key viper-vi-basic-map (kbd "C-b") nil)
  (define-key viper-vi-basic-map (kbd "C-f") nil)
  (define-key viper-vi-basic-map (kbd "C-n") nil)
  (define-key viper-vi-basic-map (kbd "C-p") nil)
  (define-key viper-vi-basic-map (kbd "C-q") nil))

;; vimpulse patches
