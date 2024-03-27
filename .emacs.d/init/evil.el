(eval-when-compile (require 'cl-lib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load

(setq-default evil-auto-indent t
              evil-shift-with 4
              evil-cross-lines t
              evil-echo-state nil
              evil-want-C-i-jump nil
              evil-want-fine-undo t
              evil-search-module 'evil-search
              evil-ex-search-vim-style-regexp nil ; we use PCRE
              evil-undo-system 'undo-tree
              )

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
  `(cl-loop for spec in ',definition
            for swap = (eq (nth 0 spec) 'swap)
            for del = (eq (nth 0 spec) 'delete)
            when (or swap del) do (pop spec) end
            for type = (nth 0 spec)
            do (cl-loop for type in (or (and (listp type) type) (list type))
                        for map = (symbol-value
                                   (or (cdr (assq type evil-misc-map-alist))
                                       (evil-state-property type :keymap)))
                        do (cl-loop for (key def) on (cdr spec) by (if del #'cdr #'cddr)
                                    when del do (setq def nil) end
                                    for elt = (cons (read-kbd-macro key) def)
                                    collect elt into defs
                                    when swap
                                    do (let ((key2 (read-kbd-macro def)))
                                         (setcdr elt (lookup-key map def))
                                         (push (cons key2 (lookup-key map key)) defs))
                                    end
                                    finally
                                    do (cl-loop for (key . def) in defs
                                                do (define-key map key def))))))

;; dependencies
(bundle goto-chg)
(bundle tarao-elisp)
(bundle pcre2el)

(bundle! evil
  (evil-mode 1)

  (evil-define-keys
   ;; use ; for : as
   ;;   noremap ; :
   ;; in Vim
   (motion ";"   evil-ex)

   (normal "M-." evil-goto-definition)
   (motion ":"   tarao/helm-for-files
           "M-;" tarao/helm-for-files
           "C-q" helm-ghq
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

   ;; frame commands
   (window "f h" clone-frame-to-left)
   (window "f <" clone-frame-to-left)
   (window "f l" clone-frame-to-right)
   (window "f >" clone-frame-to-right)
   (window "f d" delete-frame)

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

  ;; A better regexp
  (defadvice evil-ex-make-pattern
    (before transform-pcre (regexp case whole-line) activate)
    "Transform PCRE into Elisp regexp."
    (let ((re (ignore-errors (rxt-pcre-to-elisp regexp))))
      (when re (setq regexp re))))

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

(bundle evil-leader
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "gh" 'open-github-from-file
    "gb" 'git-messenger:popup-message
    "gg" 'helm-git-grep
    "gp" 'helm-git-grep-at-point
    "gs" 'magit-status
    "gi" 'magit-status
    "gco" 'helm-magit:checkout
    "gd" 'helm-magit:diff)
  (global-evil-leader-mode))

(bundle color-moccur)
(bundle tarao-evil-plugins
  :features (evil-mode-line evil-relative-linum
             evil-little-word hexl-evil-patch)

  ;; operators

  (global-evil-operator-comment-mode 1)

  (global-evil-operator-moccur-mode 1)
  (with-eval-after-load-feature 'color-moccur
    (define-key moccur-mode-map (kbd ":") #'tarao/helm-for-files)
    (define-key moccur-mode-map (kbd "M-;") #'tarao/helm-for-files))

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
    (when (and (eq evil-state 'insert) (bound-and-true-p skk-j-mode))
      ad-do-it))
  (defadvice evil-refresh-cursor
    (around evil-refresh-cursor-unless-skk-mode activate)
    "Allow ccc to update cursor color only when we are in insert
  state and in `skk-j-mode'."
    (unless (and (eq evil-state 'insert) (bound-and-true-p skk-j-mode))
      ad-do-it))
  (defadvice evil-ex-search-update-pattern
    (around evil-inhibit-ex-search-update-pattern-in-skk-henkan activate)
    "Inhibit search pattern update during `skk-henkan-mode'.
  This is reasonable since inserted text during `skk-henkan-mode'
  is a kind of temporary one which is not confirmed yet."
    (unless (bound-and-true-p skk-henkan-mode)
      ad-do-it))

  ;; view mode
  (with-eval-after-load-feature (view)
    (add-hook 'view-mode-hook
              #'(lambda ()
                  (evil-initialize-state)
                  (evil-make-overriding-map view-mode-map 'normal)))
    (evil-define-key 'motion view-mode-map
      (kbd "v") #'(lambda () (interactive) (view-mode 0))))

  ;; special-mode
  (evil-define-key 'normal special-mode-map "q" #'quit-window)

  ;; xref
  (with-eval-after-load-feature (xref)
    (evil-make-overriding-map xref--xref-buffer-mode-map 'normal))

  ;; dired mode
  (with-eval-after-load-feature (dired)
    (evil-define-key 'normal dired-mode-map "c" #'dired-do-copy))

  ;; treemacs
  (with-eval-after-load-feature (treemacs)
    (evil-make-overriding-map treemacs-mode-map 'normal)
    (evil-add-hjkl-bindings treemacs-mode-map 'normal))

  ;; comint
  (evil-define-key 'insert comint-mode-map
    (kbd "C-p") #'comint-previous-input
    (kbd "C-n") #'comint-next-input
    (kbd "C-u") #'comint-kill-input)

  ;; compilation
  (evil-add-hjkl-bindings compilation-mode-map 'motion
    (kbd "gg") #'evil-goto-first-line
    (kbd "n") #'(lambda ()
                  (interactive)
                  (if (null evil-ex-active-highlights-alist)
                      (call-interactively 'compilation-next-error)
                    (call-interactively 'evil-ex-search-next)))
    (kbd "N") #'(lambda ()
                  (interactive)
                  (if (null evil-ex-active-highlights-alist)
                      (call-interactively 'compilation-previous-error)
                    (call-interactively 'evil-ex-search-previous))))
  (evil-add-hjkl-bindings compilation-mode-map 'visual)

  ;; git-messenger
  (defmacro with-passing-through-git-messenger-popup (command)
    (let ((post-command
           `(lambda ()
              (call-interactively #',command)
              (git-messenger:popup-message))))
      `(progn
         (run-with-timer 0 nil ',post-command)
         (git-messenger:popup-close))))
  (defmacro pass-through-git-messenger-key (state key)
    (let* ((map (symbol-value (evil-state-property state :keymap)))
           (command (lookup-key map key)))
      `(progn
         (define-key git-messenger-map (kbd ,key)
           #'(lambda ()
               (interactive)
               (with-passing-through-git-messenger-popup ,command))))))
  (with-eval-after-load-feature (git-messenger)
    (pass-through-git-messenger-key motion "j")
    (pass-through-git-messenger-key motion "k")
    (pass-through-git-messenger-key motion "J")
    (pass-through-git-messenger-key motion "K")
    (let ((map git-messenger-map))
      (define-key map (kbd "C-y") 'evil-scroll-line-up)
      (define-key map (kbd "C-e") 'evil-scroll-line-down)
      (define-key map (kbd "C-u") 'evil-scroll-up)
      (define-key map (kbd "C-d") 'evil-scroll-down)))

  ;; magit
  (with-eval-after-load-feature (magit-mode)
    (let ((map magit-mode-map))
      (define-key map "J" 'magit-section-forward-sibling)
      (define-key map "K" 'magit-section-backward-sibling)
      (define-key map "j" 'evil-next-line)
      (define-key map "k" 'evil-previous-line)
      (define-key map ":" #'tarao/helm-for-files)
      (define-key map (kbd "C-y") 'evil-scroll-line-up)
      (define-key map (kbd "C-e") 'evil-scroll-line-down)
      (define-key map (kbd "C-u") 'evil-scroll-up)
      (define-key map (kbd "C-d") 'evil-scroll-down)
      (define-key map (kbd "C-w") 'evil-window-map)))
  (with-eval-after-load-feature 'magit-diff
    (let ((map magit-diff-mode-map))
      (define-key map "j" 'evil-next-line)
      (define-key map "k" 'evil-previous-line))
    (define-key magit-file-section-map "K" 'magit-section-backward-sibling))
  (with-eval-after-load-feature 'magit
    (let ((map magit-status-mode-map))
      (define-key map "j" 'evil-next-line)))

  ;; multi-mode
  (with-eval-after-load-feature 'multi-mode-util
    (bundle multi-mode+evil))

  ;; cvim
  (add-hook 'cvim-edit:local-mode-hook #'(lambda () (evil-append-line 1)))

  ;; LSP

  (defmacro install-lsp-bindings (lang)
    (let ((mode (intern (format "%s-mode" lang)))
          (map (intern (format "%s-mode-map" lang))))
      `(progn
         (with-eval-after-load-feature ',mode
           (evil-define-key 'normal ,map
             (kbd "M-.") #'lsp-find-definition
             ))
         (let ((bindings
                '(
                  "/" helm-lsp-workspace-symbol

                  "i" lsp-describe-thing-at-point

                  "=" lsp-format-buffer

                  "fr" lsp-find-references
                  "fd" lsp-find-declaration
                  "fi" lsp-find-implementation
                  "ft" lsp-find-type-definition

                  "pr" lsp-ui-peek-find-references
                  "pd" lsp-ui-peek-find-definitions
                  "pi" lsp-ui-peek-find-implementation
                  "ps" lsp-ui-peek-find-workspace-symbol

                  "lc" lsp-ui-flycheck-list
                  "li" lsp-ui-imenu
                  "ls" lsp-treemacs-symbols
                  "ll" lsp-avy-lens

                  "rr" lsp-rename
                  "ri" lsp-organize-imports

                  "ax" lsp-execute-code-action
                  "ah" helm-lsp-code-actions

                  "ds" dap-debug
                  "dS" dap-disconnect
                  "di" dap-step-in
                  "do" dap-step-out
                  "dn" dap-next
                  "dc" dap-continue
                  "db" dap-breakpoint-toggle
                  "de" dap-ui-expressions-add
                  "dE" dap-ui-expressions-remove
                  "dwb" dap-ui-breakpoints
                  "dws" dap-ui-sessions
                  "dwe" dap-ui-expressions
                  "dwl" dap-ui-locals
                  )))
           (apply 'evil-leader/set-key-for-mode ',mode bindings)))))

  (with-eval-after-load-feature (lsp-ui-imenu)
    (evil-make-overriding-map lsp-ui-imenu-mode-map 'normal))

  (with-eval-after-load-feature (lsp-ui-flycheck)
    (evil-make-overriding-map lsp-ui-flycheck-list-mode-map 'normal))

  ;; scala

  (install-lsp-bindings scala)
  (add-to-list 'evil-insert-state-modes 'sbt-mode)

  (with-eval-after-load-feature 'scala-mode
    (defun tarao/scala-join-line ()
      "Adapt `scala-indent:join-line' to behave more like evil's
  line join. `scala-indent:join-line' acts like the vanilla
  `join-line', joining the current line with the previous
  one. The vimmy way is to join the current line with the next.
  Try to move to the subsequent line and then join. Then manually
  move point to the position of the join."
      (interactive)
      (let (join-pos)
        (save-excursion
          (goto-char (line-end-position))
          (unless (eobp)
            (forward-line)
            (call-interactively #'scala-indent:join-line)
            (setq join-pos (point))))
        (when join-pos
          (goto-char join-pos))))
    (evil-define-key 'normal scala-mode-map
      (kbd "C-j") #'tarao/scala-join-line
      ))
  (let ((bindings
         '(
           "mi" lsp-metals-build-import
           "md" lsp-metals-doctor-run

           "bc" bloop-compile
           "bC" bloop-clean
           "ta" bloop-test
           "to" bloop-test-only

           "repl" bloop-console
           )))
    (apply 'evil-leader/set-key-for-mode 'scala-mode bindings))

  ;; golang

  (install-lsp-bindings go)

  ;; typescript

  (install-lsp-bindings typescript)

  ;; python

  (install-lsp-bindings python)

  )

(bundle treemacs-evil)
