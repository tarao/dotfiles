;; frame title
(setq frame-title-format
      '("" invocation-name "-" emacs-version "@" system-name ": %b"))

;; theme
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

;; customize colors
(defvar mode-line-default-color "#3f3f3f")
(set-face-background 'region "#8c8ce8")
(set-face-background 'mode-line mode-line-default-color)
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

(bundle tarao-elisp
  :features (mode-line-color)
  ;; mode line color
  (mode-line-color-mode)
  (setq mode-line-color-original mode-line-default-color)
  (defvar skk-j-mode-line-color "IndianRed4")
  (defsubst skk-j-mode-line-color-p ()
    (cond
     ((and (boundp 'evil-mode) (boundp 'evil-state)
           evil-mode (not (eq evil-state 'insert)))
      nil)
     ((and (boundp 'viper-mode) (boundp 'viper-current-state)
           viper-mode (not (eq viper-current-state 'insert-state)))
      nil)
     ((and (boundp 'skk-j-mode) skk-j-mode))))
  (define-mode-line-color (color)
    (when (skk-j-mode-line-color-p)
      skk-j-mode-line-color))
  (defadvice skk-update-modeline (after ad-skk-mode-line-color activate)
    (mode-line-color-update))

  ;; eof mark
  (global-end-mark-mode))

;; scroll bar
(bundle yascroll
  (global-yascroll-bar-mode))

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
        jaspace-alternate-jaspace-string (string #x25a1)
        jaspace-mode-string " WS"))

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; parenthesis
(show-paren-mode t)

;; hide-show
(defun hs-enable ()
  (interactive)
  (hs-minor-mode)
  (local-set-key (kbd "C-c h") #'hs-hide-block)
  (local-set-key (kbd "C-c s") #'hs-show-block)
  (local-set-key (kbd "C-c l") #'hs-hide-level))
