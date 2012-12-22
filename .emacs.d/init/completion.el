;; shell command (with saving the last command for default value)
(bundle tarao-elisp
  (global-set-key (kbd "M-!") 'shell-command+)
  (global-set-key (kbd "M-|") 'shell-command-on-region+))

;; zsh like completion
(setq read-file-name-completion-ignore-case t)
(bundle! zlc :url "http://github.com/mooz/emacs-zlc.git"
  (let ((map minibuffer-local-map))
    (define-key map (kbd "C-p") 'zlc-select-previous)
    (define-key map (kbd "C-n") 'zlc-select-next)
    (define-key map (kbd "<up>") 'zlc-select-previous-vertical)
    (define-key map (kbd "<down>") 'zlc-select-next-vertical)
    (define-key map (kbd "C-u") 'backward-kill-path-element)))
(bundle tarao-elisp
  (yaicomplete-mode))

;; anything
(setq dired-bind-jump nil)
(bundle anything :build ("make")
  (setq anything-enable-shortcuts 'alphabet
        anything-for-files-prefered-list
        '(anything-c-source-buffers+
          anything-c-source-ffap-line
          anything-c-source-ffap-guesser
          anything-c-source-recentf
          anything-c-source-bookmarks
          anything-c-source-files-in-current-dir+
          anything-c-source-locate)
        anything-complete-sort-candidates t)
  (substitute-key-definition 'execute-extended-command
                             'anything-execute-extended-command global-map))

(bundle descbinds-anything
  (descbinds-anything-install)
  (global-set-key (kbd "C-x b") 'anything-for-files)
  (define-key anything-map (kbd "M-n") 'anything-next-source)
  (define-key anything-map (kbd "M-p") 'anything-previous-source))

;; auto completion like IntelliSense
(bundle auto-complete
  (global-auto-complete-mode t)
  (setq ac-auto-show-menu 0.5)
  (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
  (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
  (define-key ac-complete-mode-map (kbd "TAB") nil))
