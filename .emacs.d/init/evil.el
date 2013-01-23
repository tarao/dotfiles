;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load

(setq-default evil-auto-indent t
              evil-shift-with 4
              evil-cross-lines t
              evil-echo-state nil
              evil-want-C-i-jump nil
              evil-want-fine-undo t
              evil-search-module 'evil-search
              evil-ex-search-vim-style-regexp t)

;; dependencies
(bundle anything)
(bundle goto-chg)
(bundle tarao-elisp)

(bundle evil
  (evil-mode 1)

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
  (define-key evil-motion-state-map (kbd ";") #'evil-ex)

  ;; user key bindings
  (define-key evil-motion-state-map (kbd ":") #'anything-for-files)
  (define-key evil-motion-state-map (kbd "M-;") #'anything-for-files)
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

  ;; use raw key bindings in moccur
  (push 'moccur-grep-mode evil-emacs-state-modes)

  ) ;; bundle evil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plugins

(bundle evil-surround ;; surround operator
  (global-surround-mode 1))

(bundle color-moccur)
(bundle tarao-evil-plugins
  :features (evil-mode-line evil-relative-linum
             evil-little-word hexl-evil-patch)

  ;; operators

  (global-evil-operator-comment-mode 1)

  (global-evil-operator-moccur-mode 1)
  (eval-after-load-compile 'color-moccur
    (define-key moccur-mode-map (kbd ":") #'anything-for-files)
    (define-key moccur-mode-map (kbd "M-;") #'anything-for-files))

  ;; text objects

  (define-key evil-outer-text-objects-map "f" #'evil-a-between)
  (define-key evil-inner-text-objects-map "f" #'evil-inner-between)

  ;; key bindings

  (evil-define-command-line-key (kbd "C-r") #'evil-ex-paste-from-register)

  ;; others

  ;; w for Japanese phrase
  ;; lw for Japanese word
  (setq evil-cjk-word-separating-categories word-separating-categories
        evil-cjk-word-combining-categories word-combining-categories))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; patches

;; auto-complete
(eval-after-load-compile 'auto-complete
  ;; exit insert-state by ESC even if auto-complete is showing candidates
  (define-key ac-completing-map (kbd "ESC") nil))

;; yaicomplete
(eval-after-load-compile 'yaicomplete
  ;; inhibit yaicomplete in ex-mode minibuffer
  (add-to-list 'yaicomplete-exclude 'evil-ex-current-buffer))

;; skk
(defadvice update-buffer-local-cursor-color
  (around evil-update-buffer-local-cursor-color-in-insert-state activate)
  "Allow ccc to update cursor color only when we are in insert
state and in `skk-j-mode'."
  (when (and (eq evil-state 'insert) (boundp 'skk-j-mode) skk-j-mode)
    ad-do-it))
(defadvice evil-refresh-cursor
  (around evil-refresh-cursor-unless-skk-mode activate)
  "Allow ccc to update cursor color only when we are in insert
state and in `skk-j-mode'."
  (unless (and (eq evil-state 'insert) (boundp 'skk-j-mode) skk-j-mode)
    ad-do-it))
(defadvice evil-ex-search-update-pattern
  (around evil-inhibit-ex-search-update-pattern-in-skk-henkan activate)
  "Inhibit search pattern update during `skk-henkan-mode'.
This is reasonable since inserted text during `skk-henkan-mode'
is a kind of temporary one which is not confirmed yet."
  (unless (and (boundp 'skk-henkan-mode) skk-henkan-mode)
    ad-do-it))

;; view mode
(add-hook 'view-mode-hook #'evil-initialize-state)
(eval-after-load-compile 'view
  (evil-define-key 'motion view-mode-map (kbd "v")
    #'(lambda () (interactive) (view-mode 0))))

;; dired mode
(eval-after-load-compile 'dired
  (evil-define-key 'normal dired-mode-map "c" #'dired-do-copy))

;; multi-mode
(eval-after-load-compile 'multi-mode-util
  (bundle multi-mode+evil))

;; howm
(eval-after-load-compile 'howm
  ;; menu
  (evil-make-overriding-map howm-menu-mode-map 'normal)
  (add-hook 'howm-menu-hook
            #'(lambda () (define-key howm-menu-mode-local-map ":" nil)))

  ;; list
  (evil-make-overriding-map howm-view-summary-mode-map 'normal)
  (evil-define-key 'normal howm-view-summary-mode-map
    "j" (lookup-key evil-motion-state-map "j")
    "k" (lookup-key evil-motion-state-map "k")
    "J" 'evil-scroll-down
    "K" 'evil-scroll-up))

;; mew
(defun mew-draft-evil-open-below (count)
  (interactive "p")
  (if (get-text-property (point) 'read-only)
      (progn
        (forward-line count)
        (evil-open-above 1))
    (evil-open-below count)))
(eval-after-load-compile 'mew-key
  ;; mew-summary-mode key maps
  (evil-make-overriding-map mew-summary-mode-map 'normal)
  (evil-add-hjkl-bindings mew-summary-mode-map 'normal
    "h" (lookup-key mew-summary-mode-map "h")
    "l" (lookup-key mew-summary-mode-map "l")
    "G" (lookup-key evil-motion-state-map "G")
    "J" (lookup-key evil-motion-state-map "J")
    "K" (lookup-key evil-motion-state-map "K")
    ";" (lookup-key evil-motion-state-map ";"))
  ;; mew-message-mode key maps
  (evil-make-overriding-map mew-message-mode-map 'normal)
  (evil-add-hjkl-bindings mew-message-mode-map 'normal)
  ;; mew-draft-mode key maps
  (dolist (map (list mew-draft-header-map mew-draft-body-map))
    (evil-define-key 'normal map
      "o" #'mew-draft-evil-open-below
      "q" #'mew-draft-kill)))

;; hatena-diary
(push 'hatena:d:list-mode evil-motion-state-modes)
(eval-after-load-compile 'hatena-diary
  (evil-make-overriding-map hatena:d:list-mode-map)
  (evil-add-hjkl-bindings hatena:d:list-mode-map 'motion))
