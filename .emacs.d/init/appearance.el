;; coloring
(setq frame-background-mode 'dark)
(if (not (and (>= emacs-major-version 24) (>= emacs-minor-version 1)))
    (bundle color-theme
      (color-theme-initialize)
      (color-theme-dark-laptop)
      (set-face-foreground 'mode-line "#8fb28f")
      (set-face-background 'mode-line-buffer-id "#3f3f3f")
      (set-face-foreground 'mode-line-buffer-id "#f0dfaf"))
  (bundle zenburn-theme
    :url "http://raw.github.com/bbatsov/zenburn-emacs/master/zenburn-theme.el"
    (load-theme 'zenburn t)
    (let ((class '((class color) (min-colors 89)))
          (fg "#dcdccc") (bg "#1f1f1f"))
      (custom-theme-set-faces
       'zenburn
       `(default ((,class (:foreground ,fg :background ,bg))))))))

;; colors
(set-face-background 'region "#8c8ce8")
(set-face-background 'mode-line "#3f3f3f")
(set-face-attribute 'mode-line nil :box nil)
(set-face-background 'mode-line-inactive "#5f5f5f")
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-background 'header-line "#3f3f3f")
(set-face-attribute 'header-line nil :box nil)

;; use darker comment
(defun set-comment-color (color)
  (set-face-foreground 'font-lock-comment-delimiter-face color)
  (set-face-foreground 'font-lock-comment-face color))
(defun darken-comment ()
  (interactive)
  (set-comment-color "gray32"))
(defun lighten-comment ()
  (interactive)
  (set-comment-color "OrangeRed"))
(darken-comment)

;; mode line color
(bundle tarao-elisp
  (mode-line-color-mode)
  (defvar skk-j-mode-line-color "IndianRed4")
  (define-mode-line-color (color)
    (when (and (featurep 'skk) skk-j-mode
               (or (not (featurep 'viper))
                   (not viper-mode)
                   (eq viper-current-state 'insert-state))
               (or (not (featurep 'evil))
                   (not evil-mode)
                   (eq evil-state 'insert)))
      skk-j-mode-line-color))
  (defadvice skk-update-modeline (after ad-skk-mode-line-color activate)
    (mode-line-color-update)))

;; eof mark
(bundle tarao-elisp
  (global-end-mark-mode))

;; line-wrap character
(require 'disp-table)
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
(bundle jaspace :url "http://homepage3.nifty.com/satomii/software/jaspace.el"
  (setq jaspace-highlight-tabs t
        jaspace-highlight-tabs ?>
        jaspace-mode-string " WS"))

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; parenthesis
(show-paren-mode t)
