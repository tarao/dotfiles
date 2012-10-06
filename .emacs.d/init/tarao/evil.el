;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load

(setq-default evil-auto-indent t
              evil-shift-with 4
              evil-cross-lines t
              evil-echo-state nil
              evil-want-C-i-jump nil
              evil-want-fine-undo t
              evil-search-module 'evil-search)
(require 'goto-chg nil t)
(require 'evil)
(evil-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode line

;; mode-line color
(setq evil-mode-line-color
      `((normal   . ,(face-background 'mode-line))
        (insert   . "#575735")
        (replace  . "#575735")
        (operator . "DarkSeaGreen4")
        (visual   . "SteelBlue4")
        (emacs    . "#8c5353")))
(when (featurep 'mode-line-color)
  (define-mode-line-color (color)
    (unless color (cdr (assq evil-state evil-mode-line-color)))))

;; mode-line
(defun my-evil-state-msg (&optional state)
  (unless state (setq state evil-state))
  (let ((sym (intern (concat "evil-" (symbol-name state) "-state-msg"))))
    (cond
     ((boundp sym) (symbol-value sym))
     ((evil-visual-state-p)
      (or (cdr (assq (evil-visual-type) evil-state-msg-alist))
          (cdr (assq 'normal evil-state-msg-alist))))
     (t ""))))
(defun my-evil-mode-line-format (&optional state)
  (let* ((msg (my-evil-state-msg state)) (line msg)
         (empty (= (length msg) 0)) (tail (if empty "-" "--")))
    (unless empty (setq line (concat "--" msg)))
    (list "" line tail)))
(defun my-evil-update-mode-line ()
 (condition-case ()
      (progn
        (set (make-local-variable 'my-evil-mode-line)
             (my-evil-mode-line-format))
        (when (featurep 'mode-line-color) (mode-line-color-update)))
   (error nil)))
(defadvice evil-refresh-mode-line (after ad-my-evil-update-mode-line activate)
  (my-evil-update-mode-line))
(setq evil-normal-state-msg ""
      evil-insert-state-msg "INSERT"
      evil-replace-state-msg "REPLACE"
      evil-emacs-state-msg "x"
      my-evil-mode-line (my-evil-mode-line-format 'emacs-state))
(setq-default mode-line-format
              (append '("" my-evil-mode-line) mode-line-format))
(setq evil-state-msg-alist
      '((normal . "VISUAL") (line . "VLINE") (block . "VBLOCK")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings

(defun evil-swap-key (map key1 key2)
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))
(defsubst evil-define-command-line-key (key def)
  (define-key evil-ex-completion-map key def)
  (define-key evil-ex-search-keymap key def))

;; use ; for :
(evil-define-command evil-resend-ex ()
  "Enter an Ex command."
  :keep-visual t
  (push 'evil-ex unread-command-events)
  (setq this-command last-command)
  (setq prefix-arg current-prefix-arg)
  (reset-this-command-lengths))
(define-key evil-motion-state-map (kbd ";") #'evil-resend-ex)
(define-key evil-motion-state-map [evil-ex] #'evil-ex)

;; user key bindings
(when (fboundp 'anything-for-files)
  (define-key evil-motion-state-map (kbd ":") #'anything-for-files))
(define-key evil-normal-state-map (kbd "gw") #'what-cursor-position)
(define-key evil-normal-state-map (kbd "gW") #'describe-char)
(define-key evil-normal-state-map (kbd "gA") #'describe-char)
(define-key evil-normal-state-map (kbd "C-j") #'evil-join)
(evil-swap-key evil-motion-state-map "j" "gj")
(evil-swap-key evil-motion-state-map "k" "gk")
(define-key evil-motion-state-map (kbd "J") #'evil-scroll-down)
(define-key evil-motion-state-map (kbd "K") #'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-w") #'evil-delete)

;; use default emacs key bindings
(define-key evil-normal-state-map (kbd "J") nil)
(define-key evil-insert-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-y") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key evil-insert-state-map (kbd "C-p") nil)

;; C-p and C-n works as both Emacs and Evil command
(defadvice evil-paste-pop (around ad-evil-paste-or-previous-line activate)
  (condition-case err
      ad-do-it
    (error (if (eq this-command 'evil-paste-pop)
               (call-interactively 'previous-line)
             (signal (car err) (cdr err))))))
(defadvice evil-paste-pop-next (around ad-evil-paste-or-next-line activate)
  (condition-case err
      ad-do-it
    (error (if (eq this-command 'evil-paste-pop-next)
               (call-interactively 'next-line)
             (signal (car err) (cdr err))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; patches

;; when anything-complete is active, alcs-make-candidates in
;; after-init-hook breaks the initialization of *scratch* buffer.
(defun evil-ensure-initializing-state ()
  (remove-hook 'pre-command-hook #'evil-ensure-initializing-state)
  (when evil-local-mode (evil-initialize-state)))
(add-hook 'pre-command-hook #'evil-ensure-initializing-state)

;; exit insert-state by ESC even if auto-complete is showing candidates
(when (featurep 'auto-complete)
  (define-key ac-completing-map (kbd "ESC") nil))

;; hexl
(require 'hexl-evil-patch nil t)

;; use raw key bindings in moccur
(push 'moccur-grep-mode evil-emacs-state-modes)

;; yaicomplete
(when (featurep 'yaicomplete)
  (add-to-list 'yaicomplete-exclude 'evil-ex-current-buffer))

;; skk
(eval-after-load 'ccc
  '(progn
     (defadvice update-buffer-local-cursor-color
       (around ad-update-buffer-local-cursor-color-in-insert-state activate)
       (when (and (eq evil-state 'insert) (featurep 'skk) skk-j-mode)
         ad-do-it))
     (defadvice evil-refresh-cursor
       (around ad-refresh-cursor-unless-skk-mode activate)
       (unless (and (eq evil-state 'insert) (featurep 'skk) skk-j-mode)
         ad-do-it))))
(eval-after-load 'skk
  '(progn
     (defadvice skk-mode-string-to-indicator
       (before ad-remove----from-skk-mode-string (mode string) activate)
       (when (string-match "^--" string)
         (setq string (substring string 2))))))

;; view mode
(add-hook 'view-mode-hook #'evil-initialize-state)
(evil-define-key 'motion view-mode-map (kbd "v")
  #'(lambda () (interactive) (view-mode 0)))

;; dired mode
(evil-define-key 'normal dired-mode-map "c" 'dired-do-copy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plugins

;; operators

(when (require 'surround nil t)
  (global-surround-mode 1))

(when (require 'evil-operator-comment nil t)
  (global-evil-operator-comment-mode 1))

(when (require 'evil-operator-moccur nil t)
  (global-evil-operator-moccur-mode 1)
  (when (fboundp 'anything-for-files)
    (define-key moccur-mode-map (kbd ":") 'anything-for-files)))

;; text objects

(require 'evil-textobj-between nil t)

;; others

(require 'evil-relative-linum nil t)

(when (require 'evil-little-word nil t)
  ;; w for Japanese phrase
  ;; lw for Japanese word
  (setq evil-cjk-word-separating-categories word-separating-categories)
  (setq evil-cjk-word-combining-categories word-combining-categories))

(when (require 'evil-more-registers nil t)
  (evil-define-command-line-key (kbd "C-r") #'evil-ex-paste-from-register))
