(eval-when-compile (require 'cl))

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

(defconst evil-misc-map-alist
  '((inner  . evil-inner-text-objects-map)
    (outer  . evil-inner-text-objects-map)
    (window . evil-window-map)
    (ex     . evil-ex-completion-map)
    (search . evil-ex-search-keymap)
    (read   . evil-read-key-map)))

(defmacro evil-define-keys (&rest definition)
  "Define keys for Evil keymaps by DEFINITION.

DEFINITION is a list whose element has one of the following forms:
  (TYPE KEY1 DEF1 KEY2 DEF2...)
  ((TYPE1 TYPE2...) KEY1 DEF1 KEY2 DEF2...)
  (swap TYPE KEY1 KEY1' KEY2 KEY2'...)
  (swap (TYPE1 TYPE2...) KEY1 KEY1' KEY2 KEY2'...)
  (delete TYPE KEY1 KEY2...)
  (delete (TYPE1 TYPE2...) KEY1 KEY2...)

TYPE is either a state or one of `inner', `outer', `window',
`ex', `search' or `read'."
  `(loop for spec in ',definition
         for swap = (eq (nth 0 spec) 'swap)
         for del = (eq (nth 0 spec) 'delete)
         when (or swap del) do (pop spec) end
         for type = (nth 0 spec)
         do (loop for type in (or (and (listp type) type) (list type))
                  for map = (symbol-value
                             (or (cdr (assq type evil-misc-map-alist))
                                 (evil-state-property type :keymap)))
                  do (loop for (key def) on (cdr spec) by (if del #'cdr #'cddr)
                           when del do (setq def nil) end
                           for elt = (cons (read-kbd-macro key) def)
                           collect elt into defs
                           when swap
                           do (let ((key2 (read-kbd-macro def)))
                                (setcdr elt (lookup-key map def))
                                (push (cons key2 (lookup-key map key)) defs))
                           end
                           finally
                           do (loop for (key . def) in defs
                                    do (define-key map key def))))))

;; dependencies
(bundle anything)
(bundle goto-chg)
(bundle tarao-elisp)

(bundle! evil
  (evil-mode 1)

  (evil-define-keys
   ;; use ; for : as
   ;;   noremap ; :
   ;; in Vim
   (motion ";"   evil-ex)

   (motion ":"   tarao/anything-for-files
           "M-;" tarao/anything-for-files
           "gw"  what-cursor-position
           "gW"  describe-char
           "gA"  describe-char)

   ;; move cursor visually by default
   (swap motion  "j" "gj"  "k" "gk")

   ;; use C-j instead of J because we override J by `evil-scroll-down'
   (normal "J"   nil
           "C-j" evil-join)
   (motion "J"   evil-scroll-down
           "K"   evil-scroll-up)

   ;; `evil-window-map' in visual state does not make sense
   (visual "C-w" evil-delete)

   ;; use default emacs key bindings
   (delete insert "C-e" "C-y" "C-k" "C-n" "C-p" "C-t" "C-d")
   (delete ex     "C-a" "C-b")

   ) ;; evil-define-keys

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

;; After this, inside `bundle', we can assume that Evil is installed
;; and loaded.  It is not the case outside `bundle' since `(bundle
;; evil ...)'  installs Evil but this is not evaluated at compile
;; time.  Initialization code in `bundle' is evaluated when `(bundle
;; ...)' is evaluated, so that the initialization code runs after
;; preceding `(bundle ...)' calls.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plugins

(bundle evil-surround ;; surround operator
  (global-evil-surround-mode 1))

(bundle color-moccur)
(bundle tarao-evil-plugins
  :features (evil-mode-line evil-relative-linum
             evil-little-word hexl-evil-patch)

  ;; operators

  (global-evil-operator-comment-mode 1)

  (global-evil-operator-moccur-mode 1)
  (with-eval-after-load-feature 'color-moccur
    (define-key moccur-mode-map (kbd ":") #'tarao/anything-for-files)
    (define-key moccur-mode-map (kbd "M-;") #'tarao/anything-for-files))

  ;; key bindings

  (evil-define-keys
   (outer "f" evil-a-between)
   (inner "f" evil-inner-between)
   ((ex search) "C-r" evil-ex-paste-from-register)
   ) ;; evil-define-keys

  ;; others

  ;; w for Japanese phrase
  ;; lw for Japanese word
  (setq evil-cjk-word-separating-categories word-separating-categories
        evil-cjk-word-combining-categories word-combining-categories))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration

(bundle evil
  ;; yaicomplete
  (with-eval-after-load-feature 'yaicomplete
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
  (with-eval-after-load-feature (view)
    (evil-define-key 'motion view-mode-map (kbd "v")
      #'(lambda () (interactive) (view-mode 0))))

  ;; dired mode
  (with-eval-after-load-feature (dired)
    (evil-define-key 'normal dired-mode-map "c" #'dired-do-copy))

  ;; multi-mode
  (with-eval-after-load-feature 'multi-mode-util
    (bundle multi-mode+evil))

  ;; howm
  (with-eval-after-load-feature (howm)
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
  (with-eval-after-load-feature 'mew-key
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
  (with-eval-after-load-feature (evil hatena-diary)
    (evil-make-overriding-map hatena:d:list-mode-map)
    (evil-add-hjkl-bindings hatena:d:list-mode-map 'motion))
  )
