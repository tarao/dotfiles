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

;; show fullwidth-spaces and tabs
(require 'jaspace)
(setq jaspace-highlight-tabs t)
(setq jaspace-highlight-tabs ?>)
(setq jaspace-mode-string " WS")

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; parenthesis
(show-paren-mode t)
