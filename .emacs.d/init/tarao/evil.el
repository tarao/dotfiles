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

;; mode-line color corresponds to state
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
  "Find a message string for STATE.
If `evil-STATE-state-msg' is bound, use that value.  Otherwise,
if STATE is a visual state, then `evil-state-msg-alist' is looked
up by the return value of `evil-visual-type'.  If no message
string is found, return an empty string."
  (unless state (setq state evil-state))
  (let ((sym (intern (concat "evil-" (symbol-name state) "-state-msg"))))
    (cond
     ((boundp sym) (symbol-value sym))
     ((evil-visual-state-p)
      (or (cdr (assq (evil-visual-type) evil-state-msg-alist))
          (cdr (assq 'normal evil-state-msg-alist))))
     (t ""))))
(defun my-evil-mode-line-format (&optional state)
  "Make mode string for STATE.
If `my-evil-state-msg' returns non-empty string, the mode string
is \"--STATE MESSAGE--\".  Otherwise, the mode string is \"-\"."
  (let* ((msg (my-evil-state-msg state)) (line msg)
         (empty (= (length msg) 0)) (tail (if empty "-" "--")))
    (unless empty (setq line (concat "--" msg)))
    (list "" line tail)))
(eval-after-load 'skk
  '(progn
     (defadvice skk-mode-string-to-indicator
       (before my-evil-remove----from-skk-mode-string (mode string) activate)
       "Do not put \"--\" at the beginning of mode string.
We have our own \"--\" put by `my-evil-mode-line-format'."
       (when (string-match "^--" string)
         (setq string (substring string 2))))))
(defun my-evil-update-mode-line ()
  "Update `my-evil-mode-line' and update mode line color."
  (condition-case ()
      (progn
        (set (make-local-variable 'my-evil-mode-line)
             (my-evil-mode-line-format))
        (when (featurep 'mode-line-color) (mode-line-color-update)))
   (error nil)))
(defadvice evil-refresh-mode-line (after my-evil-update-mode-line activate)
  "Update our own mode string by `my-evil-update-mode-line'."
  (my-evil-update-mode-line))
(defconst evil-normal-state-msg "")
(defconst evil-insert-state-msg "INSERT")
(defconst evil-replace-state-msg "REPLACE")
(defconst evil-emacs-state-msg "x")
(defconst evil-state-msg-alist
  '((normal . "VISUAL") (line . "VLINE") (block . "VBLOCK")))
(defvar my-evil-mode-line (my-evil-mode-line-format 'emacs-state))
(setq-default mode-line-format
              (append '("" my-evil-mode-line) mode-line-format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings

(defun evil-swap-key (map key1 key2)
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))
(defsubst evil-define-command-line-key (key def)
  (define-key evil-ex-completion-map key def)
  (define-key evil-ex-search-keymap key def))

;; use ; for : as
;;   noremap ; :
;; in Vim
;;
;; we can simply do
;;   (define-key evil-motion-state (kbd ";") #'evil-ex)
;; but in this case, unlike Vim, ";" is shown as a prompt
;;
;; the following hack binds `evil-ex' to another key
;; with preserving the prompt string
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
;; use C-j instead of J because we override J by `evil-scroll-down'
(define-key evil-normal-state-map (kbd "C-j") #'evil-join)
;; move cursor visually by default
(evil-swap-key evil-motion-state-map "j" "gj")
(evil-swap-key evil-motion-state-map "k" "gk")
(define-key evil-motion-state-map (kbd "J") #'evil-scroll-down)
(define-key evil-motion-state-map (kbd "K") #'evil-scroll-up)
;; `evil-window-map' in visual state does not make sense
(define-key evil-visual-state-map (kbd "C-w") #'evil-delete)

;; use default emacs key bindings
(define-key evil-normal-state-map (kbd "J") nil)
(define-key evil-insert-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-y") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key evil-insert-state-map (kbd "C-p") nil)

;; C-p and C-n works as both Emacs and Evil command
(defadvice evil-paste-pop (around evil-paste-or-move-line activate)
  "If there is no just-yanked stretch of killed text, just move
to previous line."
  (condition-case err
      ad-do-it
    (error (if (eq this-command 'evil-paste-pop)
               (call-interactively 'previous-line)
             (signal (car err) (cdr err))))))
(defadvice evil-paste-pop-next (around evil-paste-or-move-line activate)
  "If there is no just-yanked stretch of killed text, just move
to next line."
  (condition-case err
      ad-do-it
    (error (if (eq this-command 'evil-paste-pop-next)
               (call-interactively 'next-line)
             (signal (car err) (cdr err))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; patches

(defun evil-ensure-initializing-state ()
  "Ensure calling `evil-initialize-state'.
When anything-complete is active, `alcs-make-candidates' in
`after-init-hook' breaks the initialization of *scratch* buffer."
  (remove-hook 'pre-command-hook #'evil-ensure-initializing-state)
  (when evil-local-mode (evil-initialize-state)))
(add-hook 'pre-command-hook #'evil-ensure-initializing-state)

;; auto-complete
(when (featurep 'auto-complete)
  ;; exit insert-state by ESC even if auto-complete is showing candidates
  (define-key ac-completing-map (kbd "ESC") nil))

;; hexl
(require 'hexl-evil-patch nil t)

;; use raw key bindings in moccur
(push 'moccur-grep-mode evil-emacs-state-modes)

;; yaicomplete
(when (featurep 'yaicomplete)
  ;; inhibit yaicomplete in ex-mode minibuffer
  (add-to-list 'yaicomplete-exclude 'evil-ex-current-buffer))

;; skk
(eval-after-load 'ccc
  '(progn
     (defadvice update-buffer-local-cursor-color
       (around evil-update-buffer-local-cursor-color-in-insert-state activate)
       "Allow ccc to update cursor color only when we are in
insert state and in `skk-j-mode'."
       (when (and (eq evil-state 'insert) (featurep 'skk) skk-j-mode)
         ad-do-it))
     (defadvice evil-refresh-cursor
       (around evil-refresh-cursor-unless-skk-mode activate)
       "Allow ccc to update cursor color only when we are in
insert state and in `skk-j-mode'."
       (unless (and (eq evil-state 'insert) (featurep 'skk) skk-j-mode)
         ad-do-it))))
(eval-after-load 'skk
  '(progn
     (defadvice evil-ex-search-update-pattern
       (around evil-inhibit-ex-search-update-pattern-in-skk-henkan activate)
       "Inhibit search pattern update during `skk-henkan-mode'.
This is reasonable since inserted text during `skk-henkan-mode'
is a kind of temporary one which is not confirmed yet."
       (when (not skk-henkan-mode)
         ad-do-it))))

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
