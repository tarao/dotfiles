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

;; auto completion like IntelliSense
(bundle! auto-complete
  (global-auto-complete-mode t)
  (setq ac-auto-show-menu 0.5)
  (let ((map ac-complete-mode-map))
    (define-key map (kbd "C-n") 'ac-next)
    (define-key map (kbd "C-p") 'ac-previous)
    (define-key map (kbd "TAB") nil)))

;; company
(setq-default company-lighter-base "comp")
(bundle company-mode
  ;; permanently replace `company-complete-common' with
  ;; `company-complete-common-or-cycle'.
  (with-eval-after-load 'company
    (fset 'orig-company-complete-common
          (symbol-function 'company-complete-common))
    (defun company-complete-common ()
      (interactive)
      (eval-and-compile (require 'cl-lib))
      (cl-letf (((symbol-function 'company-complete-common)
                 (symbol-function 'orig-company-complete-common)))
        (call-interactively 'company-complete-common-or-cycle))))

  (with-eval-after-load-feature 'company
    (let ((map company-active-map))
      (define-key map (kbd "C-n") 'company-select-next)
      (define-key map (kbd "C-p") 'company-select-previous)
      (define-key map (kbd "C-m") 'company-complete-selection))
    (set-face-background 'company-tooltip-selection "#4C7073")))
