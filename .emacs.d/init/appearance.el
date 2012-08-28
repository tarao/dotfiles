;; coloring
(setq frame-background-mode 'dark)
(if (or (< emacs-major-version 24) (not (fboundp 'load-theme)))
    (when (require 'color-theme nil t)
      (color-theme-initialize)
      (color-theme-dark-laptop)
      (set-face-background 'mode-line "#2b2b2b")
      (set-face-foreground 'mode-line "#8fb28f")
      (set-face-background 'mode-line-buffer-id "#2b2b2b")
      (set-face-foreground 'mode-line-buffer-id "#f0dfaf"))
  (load-theme 'zenburn t)
  (let ((class '((class color) (min-colors 89)))
        (zenburn-fg "#dcdccc")
        (zenburn-bg "#1f1f1f"))
    (custom-theme-set-faces
     'zenburn
     `(default ((,class (:foreground ,zenburn-fg :background ,zenburn-bg)))))))

;; colors
(set-face-background 'region "#8c8ce8")

;; mode line color
(require 'mode-line-color)
(mode-line-color-mode)
(defvar skk-j-mode-line-color "IndianRed4")
(defun skk-set-mode-line-color (setter)
  (when (and (featurep 'skk) skk-j-mode
             (or (not (featurep 'viper))
                 (not viper-mode)
                 (eq viper-current-state 'insert-state))
             (or (not (featurep 'evil))
                 (not evil-mode)
                 (eq evil-state 'insert)))
    (funcall setter skk-j-mode-line-color)))
(add-hook 'mode-line-color-hook 'skk-set-mode-line-color)
(defadvice skk-update-modeline (after ad-skk-mode-line-color activate)
  (mode-line-color-update))

;; eof mark
(require 'end-mark)
(global-end-mark-mode)

;; line-wrap character
(defface wrap-face
  '((((class color) (min-colors 88) (background dark))
     :foreground "aquamarine4")
    (((class color) (min-colors 88) (background light))
     :foreground "aquamarine2")
    (((class color) (min-colors 16))
     :foreground "DarkCyan")
    (((class color) (min-colors 8))
     :foreground "gray")
    (((type tty) (class mono))
     :inverse-video t))
  "Face of the wrap."
  :group 'convenience)
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code #xbb 'wrap-face))

;; show fullwidth-spaces and tabs
(require 'jaspace)
(setq jaspace-highlight-tabs t)
(setq jaspace-highlight-tabs ?>)
(setq jaspace-mode-string " WS")

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; parenthesis
(show-paren-mode t)
