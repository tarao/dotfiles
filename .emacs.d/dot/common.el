(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(indicate-buffer-boundaries (quote left))
 '(tool-bar-mode nil)
 '(menu-bar-mode nil))

; load-path
(setq load-path (cons "~/.emacs.d/site-lisp" load-path))
(setq load-path (cons "~/.emacs.d" load-path))

; hostname
(string-match "^\\([^\\.]+\\)\\(\\.\\(.*\\)\\)?$" (system-name))
(defconst short-hostname (replace-match "\\1" t nil (system-name))
  "Host part of function `system-name'.")

; no cursor blinking
(blink-cursor-mode nil)

; key bindings
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-j") 'enlarge-window)
(global-set-key (kbd "M-k") 'shrink-window)

; terminal fix
(global-set-key (kbd "M-O a") 'backward-paragraph)
(global-set-key (kbd "M-O b") 'forward-paragraph)
(global-set-key (kbd "M-O d") 'backward-word)
(global-set-key (kbd "M-O c") 'forward-word)
(global-set-key (kbd "<A-next>") 'scroll-other-window)
(global-set-key (kbd "<A-prior>") 'scroll-other-window-down)
(global-set-key (kbd "ESC <down>") 'enlarge-window)
(global-set-key (kbd "ESC <up>") 'shrink-window)
(global-set-key (kbd "<A-down>") 'enlarge-window)
(global-set-key (kbd "<A-up>") 'shrink-window)
(global-set-key (kbd "M-o 3 b") 'enlarge-window)
(global-set-key (kbd "M-o 3 a") 'shrink-window)
(global-set-key (kbd "M-O 3 b") 'enlarge-window)
(global-set-key (kbd "M-O 3 a") 'shrink-window)
(global-set-key (kbd "ESC M-O d") 'backward-sexp)
(global-set-key (kbd "ESC M-O c") 'forward-sexp)

; Mule-UCS settings
;-; --> disabled: not needed in Emacs 22
;-;(require 'un-define) ; Unicode
;-;(require 'jisx0213)  ; JIS X 0213

; charset info fix
(aset (charset-info 'greek-iso8859-7) 4 2)
(aset (charset-info 'cyrillic-iso8859-5) 4 2)
(aset (charset-info 'eight-bit-graphic) 4 2)

(when (and (< emacs-major-version 23) (>= emacs-major-version 22))
  ; language and charset
  ; jisx0213 support in utf-8
  (utf-translate-cjk-set-unicode-range
   '((#x00a2 . #x00a3) (#x00a7 . #x00a8) (#x00ac . #x00ac) (#x00b0 . #x00b1)
     (#x00b4 . #x00b4) (#x00b6 . #x00b6) (#x00d7 . #x00d7) (#X00f7 . #x00f7)
     (#x0370 . #x03ff) (#x0400 . #x04FF) (#x2000 . #x206F) (#x2100 . #x214F)
     (#x2190 . #x21FF) (#x2200 . #x22FF) (#x2300 . #x23FF) (#x2500 . #x257F)
     (#x25A0 . #x25FF) (#x2600 . #x26FF) (#x2e80 . #xd7a3) (#xff00 . #xffef)))
     ; for patched utf-8.el (utf-8.el.diff applied; subst-jisx0213.el required)
     ;-; --> disabled: there are problems in showing latin characters
     ;-;(modify-category-entry (make-char 'japanese-jisx0213-1) ?j)
     ;-;(modify-category-entry (make-char 'japanese-jisx0213-2) ?j)
     ;-;(eval-after-load "subst-jis" '(load "subst-jisx0213"))
     ;-;(load "utf-8") ;; patched file
     ;-;(load "utf-16") ;; for safe-charsets
     ;-;(utf-translate-cjk-set-unicode-range `((#x80 . ,(lsh -1 -1))))
)
; language and coding-system
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)
(if (equal (getenv "TERM")  "cygwin")
    (set-terminal-coding-system 'sjis)
  (set-terminal-coding-system 'utf-8-unix))
(require 'default-file-coding-systems)

; move window
(windmove-default-keybindings)
(setq windmove-wrap-around t)

; skk
(setq skk-init-file "dot/.skk")
(require 'skk-autoloads)
(global-set-key (kbd "C-x C-j") 'skk-mode)

; show line numbers
(require 'linum)
(require 'linum+)
(global-set-key (kbd "M-N") 'linum-mode)
(global-set-key (kbd "M-n") 'relative-linum-mode)

; eof mark
(require 'end-mark)
(unless window-system (global-end-mark-mode))

; parenthesis
(show-paren-mode t)

; browse-kill-ring
(autoload 'browse-kill-ring "browse-kill-ring" nil t)
(define-key global-map (kbd "M-y") 'browse-kill-ring)
(load "browse-kill-ring+")

; make *scratch* immortal
(require 'immortal-buffer)
(make-buffer-immortal "*scratch*")

; shell command (with saving the last command for default value)
(require 'shell-command+)
(global-set-key (kbd "M-!") 'shell-command+)
(global-set-key (kbd "M-|") 'shell-command-on-region+)

; completer
(require 'completer)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completer-words "---. <_")

; anything
(require 'anything-config)
(setq anything-sources
      '(
        anything-c-source-ffap-line
        anything-c-source-ffap-guesser
        anything-c-source-buffers+
        anything-c-source-file-name-history
        anything-c-source-files-in-current-dir+
        anything-c-source-locate
        anything-c-source-kill-ring
        ))
(global-set-key (kbd "C-x b") 'anything)
(define-key anything-map (kbd "C-M-n") 'anything-next-source)
(define-key anything-map (kbd "C-M-p") 'anything-previous-source)

;; describe-bindings alternatative
(require 'descbinds-anything)
(descbinds-anything-install)

; auto completion like IntelliSense
(require 'auto-complete)
(global-auto-complete-mode t)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
(define-key ac-complete-mode-map (kbd "TAB") nil)

; auto-insert
(require 'autoinsert)
(add-hook 'find-file-not-found-hooks 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/insert/")
(setq auto-insert-query nil)
(setq auto-insert-alist nil)

; M-x compile
(setq compile-command "make -k")
(setq compile-history (list "make" "make clean"))

; M-x grep
(setq grep-program "lgrep")
(setq grep-command "lgrep -inH -Au8 ")

; wc (CC/WW/LL)
(autoload 'word-count-mode "word-count" "Minor mode to count words." t nil)
(global-set-key (kbd "M-+") 'word-count-mode)

; tabbing
(setq-default tab-width 4 indent-tabs-mode nil)

; show trailing whitespace
(setq-default show-trailing-whitespace t)

; show fullwidth-spaces and tabs
(require 'jaspace)
;(setq jaspace-alternate-eol-string "\xab\n")
(setq jaspace-highlight-tabs t)
(setq jaspace-highlight-tabs ?>)

; align
(require 'align)

; ruby-mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)
                ("Rakefile$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)))
(autoload 'rdoc-mode "rdoc-mode"
  "Mode for editing rdoc files" t)
(setq auto-mode-alist
      (append '(("\\.rdoc$" . rdoc-mode)) auto-mode-alist))

; tuareg-mode - Objective Caml support
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'tuareg-run-caml "tuareg" "Run the Caml interactive compiler" t)
; font-lock
(if (and (boundp 'window-system) window-system)
    (when (string-match "XEmacs" emacs-version)
      (if (not (and (boundp 'mule-x-win-initted) mule-x-win-initted))
          (require 'sym-lock))
      (require 'font-lock)))

; haskell-mode
(load "haskell-site-file")

; css mode
(autoload 'css-mode "css-mode" "Major mode for editing CSS" t)
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))
(setq cssm-newline-before-closing-bracket t)

; nxml-mode
(load "rng-auto")
(add-to-list 'auto-mode-alist
             (cons (concat "\\."
                           (regexp-opt
                            '("xml" "xsd" "sch" "rng" "xslt"
                              "svg" "rss" "mxml") t)
                           "\\'")
                   'nxml-mode))
(unify-8859-on-decoding-mode)
(setq magic-mode-alist
      (cons '("<\\?xml " . nxml-mode)
            magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)
;; use nxml-mode instead of sgml, xml or html mode.
(mapc
 (lambda (pair)
   (if (or (eq (cdr pair) 'xml-mode)
           (eq (cdr pair) 'sgml-mode)
           (eq (cdr pair) 'html-mode))
       (setcdr pair 'nxml-mode)))
 magic-mode-alist)
;; use zen-coding
(require 'zencoding-mode)
(require 'zencoding-trie)
(add-hook 'nxml-mode-hook 'zencoding-mode)
(define-key zencoding-mode-keymap (kbd "C-j") 'zencoding-expand-line)
(define-key zencoding-preview-keymap (kbd "RET") 'zencoding-preview-accept)

; xquery mode
(autoload 'xquery-mode "xquery-mode" "Major mode for editing xquery" t)
(setq auto-mode-alist (cons '("\\.xquery$" . xquery-mode) auto-mode-alist))

(autoload 'yaml-mode "yaml-mode"
  "Mode for editing YAML files" t)
(setq auto-mode-alist
      (append '(("\\.yml$" . yaml-mode)
                ("\\.yaml$" . yaml-mode)) auto-mode-alist))

; LaTeX mode
(setq TeX-default-mode 'japanese-latex-mode)
(setq auto-mode-alist
  (append
   '(("\\.sty$" . LaTeX-mode)
     ("\\.tex$" . LaTeX-mode))
   auto-mode-alist))
(setq japanese-TeX-command-default "pTeX")
(setq japanese-LaTeX-command-default "pLaTeX")
(setq latex-run-command "platex")
(setq default-file-coding-system-alist
      (append
       '(("\\.tex$" . euc-jp-unix)
         ("\\.sty$" . euc-jp-unix)
         ("\\.bib$" . euc-jp-unix))
       default-file-coding-system-alist))
(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; luxaky
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (equal short-hostname "luxaky")
    (progn
      ; color theme
      (load "color-theme-autoloads")
      ; shell
      (setq explicit-shell-file-name "zsh")
      (setq shell-file-name "zsh")
      (setq shell-command-switch "-c")
      ; mew
      (autoload 'mew "mew" nil t)
      (autoload 'mew-send "mew" nil t)
      (setq mew-smtp-user (user-login-name))
      (setq mew-mail-domain "orezdnu.org")
      (setq mew-mailbox-type 'mbox)
      (setq mew-mbox-command "incm")
      (setq mew-mbox-command-arg
            (concat "-u -d /home/users/" (user-login-name) "/Maildir"))
      ; emacs-w3m - a text browser
      (setq w3m-init-file "dot/.emacs-w3m")
      (load "w3m")
      (add-hook 'w3m-mode-hook
                '(lambda () (setq show-trailing-whitespace nil)))
      ; lookup - search dictionary
      (autoload 'lookup "lookup" nil t)
      (autoload 'lookup-region "lookup" nil t)
      (autoload 'lookup-pattern "lookup" nil t)
      (global-set-key (kbd "C-x C-y") 'lookup)
      (global-set-key (kbd "C-x y") 'lookup-region)
      (global-set-key (kbd "C-x l") 'lookup-pattern)))
