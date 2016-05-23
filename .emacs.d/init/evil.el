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
              evil-ex-search-vim-style-regexp nil ; we use PCRE
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
(bundle pcre2el)

(bundle! evil
  (evil-mode 1)

  (evil-define-keys
   ;; use ; for : as
   ;;   noremap ; :
   ;; in Vim
   (motion ";"   evil-ex)

   (motion ":"   tarao/anything-for-files
           "M-;" tarao/anything-for-files
           "C-q" anything-ghq
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
  (evil-leader/set-leader ",")
  (global-evil-leader-mode))

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

  ;; scala

  (add-to-list 'evil-insert-state-modes 'sbt-mode)
  (defadvice ensime-search-mode (after insert-state activate)
    (evil-insert-state))

  (with-eval-after-load-feature 'scala-mode2
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
    (evil-define-key 'normal scala-mode-map (kbd "C-j") #'tarao/scala-join-line)
    )

  (defmacro with-ensime-inf-switch (&rest form)
    `(progn
       ,@form
       (ensime-inf-switch)
       (evil-insert-state)))

  (defmacro with-sbt-switch (&rest form)
    (eval-and-compile
      (require 'sbt-mode)
      (defvar sbt:submode))
    `(scala/with-project-sbt
      (let ((buf (sbt:buffer-name))
            (display-buffer-fallback-action '(display-buffer-same-window)))
        (unless (comint-check-proc buf) (sbt-start))
        (let ((submode (buffer-local-value 'sbt:submode (get-buffer buf))))
          (unless (or (eq submode 'console)
                      (eq submode 'paste-mode))
            (sbt-command "console")
            (with-current-buffer buf
              (setq sbt:submode 'console))))
        ,@form
        (switch-to-buffer buf)
        (evil-insert-state))))

  (with-eval-after-load-feature (ensime evil-leader)
    (evil-define-key 'insert ensime-mode-map
      (kbd ".") #'scala/completing-dot
      (kbd "M-.") #'ensime-edit-definition
      (kbd "M-,") #'ensime-pop-find-definition-stack)

    (evil-define-key 'normal ensime-mode-map
      (kbd "M-.") #'ensime-edit-definition
      (kbd "M-,") #'ensime-pop-find-definition-stack)

    (evil-make-overriding-map ensime-popup-buffer-map 'normal)

    (evil-define-key 'normal ensime-inspector-mode-map
      (kbd "H") #'ensime-inspector-backward-page
      (kbd "L") #'ensime-inspector-forward-page
      (kbd "d") #'ensime-inspector-browse-doc
      (kbd "q") #'ensime-popup-buffer-quit-function)

    (evil-make-overriding-map ensime-refactor-info-map 'normal)
    (evil-define-key 'normal ensime-refactor-info-map
      (kbd "RET") (lookup-key ensime-refactor-info-map (kbd "c")))

    (evil-make-overriding-map ensime-compile-result-map 'normal)
    (evil-define-key 'normal ensime-compile-result-map
      (kbd "n") #'forward-button
      (kbd "N") #'backward-button)

    (evil-make-overriding-map sbt:mode-map 'insert)
    (evil-define-key 'insert sbt:mode-map
      (kbd "C-p") #'comint-previous-input
      (kbd "C-n") #'comint-next-input
      (kbd "C-u") #'comint-kill-input)

    (defun ensime-inf-eval-buffer-switch ()
      "Send buffer content to shell and switch to it in insert mode."
      (interactive)
      (with-ensime-inf-switch (ensime-inf-eval-buffer)))

    (evil-define-operator ensime-inf-eval-region-switch (beg end)
      "Send region content to shell and switch to it in insert mode."
      :motion evil-line
      (with-ensime-inf-switch (ensime-inf-eval-region beg end)))

    (defun sbt:send-buffer-switch ()
      "Send buffer content to shell and switch to it in insert mode."
      (interactive)
      (with-sbt-switch (sbt:send-buffer)))

    (evil-define-operator sbt:send-region-switch (beg end)
      "Send region content to shell and switch to it in insert mode."
      :motion evil-line
      (with-sbt-switch (sbt:send-region beg end)))

    (evil-leader/set-key-for-mode 'scala-mode
      "/"  'ensime-search
      "?"  'ensime-scalex

      "bc" 'ensime-sbt-do-compile
      "bC" 'ensime-sbt-do-clean
      "bi" 'ensime-sbt-switch
      "bp" 'ensime-sbt-do-package
      "br" 'ensime-sbt-do-run

      "ct" 'ensime-typecheck-current-file
      "cT" 'ensime-typecheck-all

      "db" 'ensime-db-set-break
      "dB" 'ensime-db-clear-break
      "dC" 'ensime-db-clear-all-breaks
      "dc" 'ensime-db-continue
      "dd" 'ensime-db-start
      "di" 'ensime-db-inspect-value-at-point
      "dl" 'ensime-db-list-locals
      "dn" 'ensime-db-next
      "do" 'ensime-db-step-out
      "dq" 'ensime-db-quit
      "dr" 'ensime-db-run
      "ds" 'ensime-db-step
      "dt" 'ensime-db-backtrace

      "ee" 'ensime-print-errors-at-point
      "el" 'ensime-show-all-errors-and-warnings
      "es" 'ensime-stacktrace-switch

      "fu" 'ensime-show-uses-of-symbol-at-point

      "gg" 'ensime-edit-definition
      "gi" 'ensime-goto-impl
      "gt" 'ensime-goto-test

      "hh" 'ensime-show-doc-for-symbol-at-point
      "hu" 'ensime-show-uses-of-symbol-at-point
      "ht" 'ensime-print-type-at-point

      "i" 'ensime-inspect-type-at-point

      "nF" 'ensime-reload-open-files
      "ns" 'ensime
      "nS" 'ensime-gen-and-restart

      "rd" 'ensime-refactor-inline-local
      "rD" 'ensime-undo-peek
      "rf" 'ensime-format-source
      "ri" 'ensime-refactor-organize-imports
      "rm" 'ensime-refactor-extract-method
      "rr" 'ensime-refactor-rename
      "rt" 'ensime-import-type-at-point
      "ru" 'ensime-undo-peek
      "rv" 'ensime-refactor-extract-local

      "ta" 'ensime-sbt-do-test
      "tr" 'ensime-sbt-do-test-quick
      "tt" 'ensime-sbt-do-test-only

      "sa" 'ensime-inf-load-file
      "sb" 'ensime-inf-eval-buffer
      "sB" 'ensime-inf-eval-buffer-switch
      "si" 'ensime-inf-switch
      "sr" 'ensime-inf-eval-region
      "sR" 'ensime-inf-eval-region-switch

      "z"  'ensime-expand-selection-command
      )
    )
  )
