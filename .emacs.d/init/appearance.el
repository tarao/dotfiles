;; frame title
(setq frame-title-format
      '("" invocation-name "-" emacs-version "@"
        (:eval (nth 0 (split-string (system-name) "\\.")))
        ": %b"))

;; theme
(setq frame-background-mode 'dark)
(defvar default-foreground-color "#aaaaaa")
(defvar default-background-color "#1f1f1f")
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
          (fg default-foreground-color) (bg default-background-color))
      (custom-theme-set-faces
       'zenburn
       `(default ((,class (:foreground ,fg :background ,bg))))))))

;; customize colors
(defvar mode-line-default-color "#3f3f3f")
(set-face-foreground 'default default-foreground-color)
(set-face-background 'default default-background-color)
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

;; visualize whitespace
(eval-after-load-compile 'whitespace
  (setq whitespace-global-modes '(not)
        whitespace-style '(face tabs tab-mark fw-space-mark lines-tail))
  ;; tab
  (setcar (nthcdr 2 (assq 'tab-mark whitespace-display-mappings)) [?> ?\t])
  (let ((face  'whitespace-tab))
    (set-face-background face nil)
    (set-face-attribute face nil :foreground "gray30" :strike-through t))
  ;; full-width space
  (defface full-width-space
    '((((class color) (background light)) (:foreground "azure3"))
      (((class color) (background dark)) (:foreground "pink4")))
    "Face for full-width space"
    :group 'whitespace)
  (let ((fw-space-mark (make-glyph-code #x25a1 'full-width-space)))
    (add-to-list 'whitespace-display-mappings
                 `(fw-space-mark ?　 ,(vector fw-space-mark)))))
;; patch
(defsubst whitespace-char-or-glyph-code-valid-p (char)
  (let ((char (if (consp char) (car char) char)))
    (or (< char 256) (characterp char))))
(defadvice whitespace-display-vector-p (around improved-version activate)
  (let ((i (length vec)))
    (when (> i 0)
      (while (and (>= (setq i (1- i)) 0)
                  (whitespace-char-or-glyph-code-valid-p (aref vec i))))
      (setq ad-return-value (< i 0)))))
;; activate
(global-whitespace-mode)

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
