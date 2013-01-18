(bundle term+)
(bundle term+mux)

(eval-after-load-compile 'term+
  (require 'xterm-256color)
  (setq ansi-term-color-vector
        [unspecified
         "black"   "#aa0000" "#00aa00" "#aa5500"
         "#8c8ce8" "#aa00aa" "#00aaaa" "#aaaaaa"
         "#555555" "#ff5555" "#55ff55" "#ffff55"
         "#5555ff" "#ff55ff" "#55ffff" "white"])
  (bundle! term+key-intercept)
  (bundle! term+mode)
  (eval-after-load-compile 'evil (bundle! term+evil))
  (eval-after-load-compile 'anything
    (define-key term+char-map (kbd "M-;") #'anything-for-files)
    (define-key term+line-map (kbd "M-;") #'anything-for-files))
  (bundle! term+anything-shell-history))

(eval-after-load-compile 'term+mux
  (define-key term+char-map (kbd "C-z") term+mux-map)
  (setq term+mux-session-host-color-alist
        '(("io"     . "#8fb28f")
          ("argo"   . "#8fb28f")
          ("phecda" . "#8fb28f")
          ("luxaky" . "#8c8ce8")
          ("nino"   . "#8c8ce8")))
  (run-hook-with-args 'term+mux-new-session-hook
                      term+mux-default-session t))
