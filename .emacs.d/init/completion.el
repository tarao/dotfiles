(bundle tarao-elisp
  ;; shell command (with saving the last command for default value)
  (global-set-key (kbd "M-!") 'shell-command+)
  (global-set-key (kbd "M-|") 'shell-command-on-region+)

  ;; incremental completion in minibuffer
  (yaicomplete-mode))

;; zsh like completion
(setq read-file-name-completion-ignore-case t)
(bundle! zlc :url "http://github.com/mooz/emacs-zlc.git"
  (zlc-mode t)
  (let ((map minibuffer-local-map))
    (define-key map (kbd "C-p") 'zlc-select-previous)
    (define-key map (kbd "C-n") 'zlc-select-next)
    (define-key map (kbd "<up>") 'zlc-select-previous-vertical)
    (define-key map (kbd "<down>") 'zlc-select-next-vertical)
    (define-key map (kbd "C-u") 'backward-kill-path-element)))

;; anything
(bundle anything
  (setq-default anything-enable-shortcuts 'alphabet
                anything-for-files-prefered-list
                '(anything-c-source-buffers+
                  anything-c-source-ffap-line
                  anything-c-source-ffap-guesser
                  anything-c-source-recentf
                  anything-c-source-bookmarks
                  anything-c-source-files-in-current-dir+
                  anything-c-source-locate)
                anything-complete-sort-candidates t)
  (global-set-key (kbd "C-x b") #'anything-for-files)
  (eval-after-load-compile 'anything
    (define-key anything-map (kbd "M-n") #'anything-next-source)
    (define-key anything-map (kbd "M-p") #'anything-previous-source))
  (global-set-key [remap execute-extended-command]
                  #'anything-execute-extended-command))
(bundle descbinds-anything)
(bundle anything-git-project
  (defun tarao/anything-for-files ()
    (interactive)
    (require 'anything-config)
    (let* ((git-source (and (= 0 (call-process "git" nil nil nil "status"))
                            (anything-c-sources-git-project-for)))
           (other-source (or git-source
                             '(anything-c-source-recentf
                               anything-c-source-bookmarks
                               anything-c-source-files-in-current-dir+
                               anything-c-source-locate)))
           (sources `(anything-c-source-buffers+
                      anything-c-source-ffap-line
                      anything-c-source-ffap-guesser
                      ,@other-source)))
      (anything-other-buffer sources "*anything for files*"))))
(unless (fboundp 'tarao/anything-for-files)
  (fset 'tarao/anything-for-files 'anything-for-files))

;; auto completion like IntelliSense
(bundle! auto-complete
  (global-auto-complete-mode t)
  (setq ac-auto-show-menu 0.5)
  (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
  (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
  (define-key ac-complete-mode-map (kbd "TAB") nil))
