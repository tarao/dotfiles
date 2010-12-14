;; viper minor mode
(setq viper-custom-file-name (locate-library "dot/.viper")
      viper-mode t
      viper-toggle-key (kbd "C-x C-z")
      viper-ex-style-motion nil
      viper-ex-style-editing nil
      viper-fast-keyseq-timeout 0
      viper-expert-level 3
      viper-case-fold-search t
      viper-inhibit-startup-message t
      viper-u-always-undo t)
(setq-default viper-auto-indent t)

(require 'viper)

;; mode-line color
(setq viper-mode-line-color
      '((vi-state       . "white")
        (insert-state   . "khaki4")
        (replace-state  . "khaki4")
        (operator-state . "darkseagreen2")
        (visual-state   . "steelblue")
        (emacs-state    . "red")))
(when (featurep 'mode-line-color)
  (add-hook 'mode-line-color-hook
            '(lambda (setter)
               (unless mode-line-color-color
                 (funcall setter
                          (cdr (assq viper-current-state
                                     viper-mode-line-color)))))))

;; mode-line
(defun my-viper-state-id (&optional state)
  (unless state (setq state viper-current-state))
  (let ((sym (intern (concat "viper-" (symbol-name state) "-id"))))
    (cond
     ((boundp sym) (symbol-value sym))
     ((eq state 'visual-state)
      (or (cdr (assq vimpulse-visual-mode vimpulse-state-id-alist))
          (cdr (assq 'normal vimpulse-state-id-alist))))
     (t ""))))
(defun my-viper-mode-line-format (&optional state)
  (let* ((id (my-viper-state-id state)) (line id)
         (empty (= (length id) 0)) (tail (if empty "-" "--")))
    (unless empty (setq line (concat "--" id)))
    (list "" line (list 'skk-mode "" tail))))
(defun my-viper-update-mode-line ()
  (condition-case ()
      (progn
        (set (make-variable-buffer-local 'my-viper-mode-line)
             (my-viper-mode-line-format))
        (when (featurep 'mode-line-color) (mode-line-color-update)))
    (error nil)))
(defadvice viper-change-state (after ad-my-viper-update-mode-line activate)
  (my-viper-update-mode-line))
(setq viper-vi-state-id ""
      viper-insert-state-id "INSERT"
      viper-replace-state-id "REPLACE"
      viper-emacs-state-id "x"
      my-viper-mode-line (my-viper-mode-line-format 'emacs-state))
(setq-default mode-line-format
              (append '("" my-viper-mode-line) mode-line-format))
(setq vimpulse-state-id-alist
      '((normal . "VISUAL") (line . "VLINE") (block . "VBLOCK")))

;; auto-indent on o and O
(defadvice viper-open-line (after viper-open-line-with-indentation activate)
  (unless (eq major-mode 'text-mode) (indent-according-to-mode)))
(defadvice viper-Open-line (after viper-Open-line-with-indentation activate)
  (unless (eq major-mode 'text-mode) (indent-according-to-mode)))

;; vi keys in the message buffer
(save-excursion
  (set-buffer "*Messages*")
  (viper-change-state-to-vi))
(defadvice init-loader-show-log (after ad-init-loader-log-vi-keys activate)
  (viper-change-state-to-vi))

;; viper-mode patches
(require 'hexl-viper-patch nil t)
(defadvice viper-maybe-checkout (around viper-dont-ask-checkout activate) nil)

;; auto-complete patches
(when (featurep 'auto-complete)
  (define-key ac-completing-map (kbd "ESC") nil))

(setq vimpulse-want-vi-keys-in-dired t
      woman-use-own-frame nil) ; don't create new frame for manpages
(require 'vimpulse)

;; undo tree
(when (featurep 'undo-tree)
  (defadvice undo-tree-visualize
    (before ad-undo-tree-visualize-change-state-to-vi activate)
    (when (not (eq viper-current-state 'vi-state))
      (viper-change-state-to-vi)))
  (vimpulse-global-set-key 'vi-state (kbd "C-r") 'undo-tree-redo))

;; vimpulse-surround
(when (require 'vimpulse-surround nil t)
  (setq vimpulse-surround-excludes '())
  (add-hook 'after-change-major-mode-hook
            '(lambda () (when (or buffer-read-only
                                  (memq major-mode vimpulse-surround-excludes))
                          (vimpulse-surround-mode -1)))))

;; ex-commands
(setq my-viper-extra-ex-commands
      '(("o" "open")
        ("open" (anything-find-file))))

(dolist (entry my-viper-extra-ex-commands)
  (setq ex-token-alist
        (delete (assoc (car entry) ex-token-alist) ex-token-alist))
  (add-to-list 'ex-token-alist entry t))

;; operator
(require 'vimpulse-operator-comment nil t)
(require 'vimpulse-relative-linum nil t)

;; textobj
(require 'vimpulse-textobj-between nil t)

;; use ; for :
(define-key vimpulse-visual-basic-map
  (kbd ";") (lambda () (interactive) (viper-ex t)))
(define-key viper-vi-basic-map
  (kbd ";") 'viper-ex)

;; user key bindings
(vimpulse-global-set-key 'vi-state (kbd ":") 'anything-for-files)
(vimpulse-global-set-key 'vi-state (kbd "C-w") 'kill-region)
(vimpulse-global-set-key 'insert-state (kbd "C-w") 'kill-region)
(vimpulse-global-set-key 'vi-state (kbd "j") 'next-line)
(vimpulse-global-set-key 'vi-state (kbd "k") 'previous-line)
(vimpulse-global-set-key 'vi-state (kbd "J") 'viper-scroll-up)
(vimpulse-global-set-key 'vi-state (kbd "K") 'viper-scroll-down)
(vimpulse-global-set-key 'vi-state (kbd "gw") 'what-cursor-position)

;; remove key bindings
(define-key viper-vi-basic-map
  (kbd "TAB") nil)
(define-key viper-insert-basic-map
  (kbd "C-p") nil)
(define-key viper-insert-basic-map
  (kbd "C-n") nil)

;; CJK patch
(setq vimpulse-cjk-want-japanese-phrase-as-word t)
(require 'vimpulse-cjk nil t)
