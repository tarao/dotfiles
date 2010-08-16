;; coloring
(setq frame-background-mode 'dark)
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

;; no cursor blinking
(blink-cursor-mode nil)

;; eof mark
(require 'end-mark)
(unless window-system (global-end-mark-mode))

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
