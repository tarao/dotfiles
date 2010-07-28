;; shell command (with saving the last command for default value)
(require 'shell-command+)
(global-set-key (kbd "M-!") 'shell-command+)
(global-set-key (kbd "M-|") 'shell-command-on-region+)

;; completer
(require 'completer)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completer-words "---. <_")

;; anything
(require 'anything-config)
(setq anything-enable-shortcuts 'alphabet)
(setq anything-for-files-prefered-list
      '(anything-c-source-buffers+
        anything-c-source-ffap-line
        anything-c-source-ffap-guesser
        anything-c-source-recentf
        anything-c-source-bookmarks
        anything-c-source-files-in-current-dir+
        anything-c-source-locate))
(require 'anything-match-plugin)
(require 'anything-complete)
(setq anything-complete-sort-candidates t)
;; (anything-read-string-mode 1)
(substitute-key-definition 'execute-extended-command
                           'anything-execute-extended-command global-map)
(require 'anything-grep)
(defalias 'ag 'anything-grep)
(require 'descbinds-anything)
(descbinds-anything-install)
(global-set-key (kbd "C-x b") 'anything-for-files)
(define-key anything-map (kbd "M-n") 'anything-next-source)
(define-key anything-map (kbd "M-p") 'anything-previous-source)

;; auto completion like IntelliSense
(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-auto-show-menu 0.5)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
(define-key ac-complete-mode-map (kbd "TAB") nil)
