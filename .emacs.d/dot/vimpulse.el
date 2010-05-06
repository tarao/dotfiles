;; viper minor mode
(setq viper-custom-file-name (locate-library "dot/.viper"))
(setq viper-mode t)
(setq viper-toggle-key (kbd "C-x C-z"))
(setq viper-ex-style-motion nil)
(setq viper-ex-style-editing nil)
(setq viper-fast-keyseq-timeout 0)
(setq viper-expert-level 3)
(setq viper-case-fold-search t)
(setq viper-inhibit-startup-message t)
(setq viper-u-always-undo t)

(require 'viper)

;; mode-line
(defun my-viper-state-id (&optional state)
  (unless state (setq state viper-current-state))
  (let ((sym (intern (concat "viper-" (symbol-name state) "-id"))))
    (cond
     ((boundp sym) (symbol-value sym))
     ((eq state 'visual-state)
      (or (cdr (assoc vimpulse-visual-mode vimpulse-state-id-alist)) ""))
     (t ""))))
(defun my-viper-mode-line-format (&optional state)
  (let* ((id (my-viper-state-id state)) (line id)
         (empty (= (length id) 0)) (tail (if empty "-" "--")))
    (unless empty (setq line (concat "--" id)))
    (list "" line (list 'skk-mode "" tail))))
(defun my-viper-update-mode-line ()
  (condition-case ()
      (set (make-variable-buffer-local 'my-viper-mode-line)
           (my-viper-mode-line-format))
    (error nil)))
(defadvice viper-change-state (after ad-my-viper-update-mode-line activate)
  (my-viper-update-mode-line))
(setq viper-vi-state-id "")
(setq viper-insert-state-id "INSERT")
(setq viper-replace-state-id "REPLACE")
(setq viper-emacs-state-id "x")
(setq my-viper-mode-line (my-viper-mode-line-format 'emacs-state))
(setq-default mode-line-format
              (append '("" my-viper-mode-line) mode-line-format))
(setq vimpulse-state-id-alist
      '((normal . "VISUAL") (line . "VLINE") (block . "VBLOCK")))

;; viper-mode patches
(require 'hexl-viper-patch)
(defadvice viper-maybe-checkout (around viper-dont-ask-checkout activate) nil)

;; auto-complete patches
(define-key ac-completing-map (kbd "ESC") nil)

(setq woman-use-own-frame nil) ; don't create new frame for manpages
(require 'vimpulse)

;; ex-commands
(setq my-viper-extra-ex-commands
      '(("o" "open")
        ("open" (anything-find-file))))

(dolist (entry my-viper-extra-ex-commands)
  (setq ex-token-alist
        (delete (assoc (car entry) ex-token-alist) ex-token-alist))
  (add-to-list 'ex-token-alist entry t))

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
(define-key viper-vi-global-user-map
  (kbd "j") 'next-line)
(define-key viper-vi-global-user-map
  (kbd "k") 'previous-line)
(define-key viper-vi-global-user-map
  (kbd "J") 'viper-scroll-up)
(define-key viper-vi-global-user-map
  (kbd "K") 'viper-scroll-down)

;; vimpulse patches
