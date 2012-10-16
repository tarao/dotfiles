(define-key term+char-map (kbd "C-z") term+mux-map)

(when (featurep 'anything)
  (define-key term+char-map (kbd "M-;") #'anything-for-files)
  (define-key term+line-map (kbd "M-;") #'anything-for-files))

(setq ansi-term-color-vector
      [unspecified
       "black"   "#aa0000" "#00aa00" "#aa5500"
       "#8c8ce8" "#aa00aa" "#00aaaa" "#aaaaaa"
       "#555555" "#ff5555" "#55ff55" "#ffff55"
       "#5555ff" "#ff55ff" "#55ffff" "white"])

(setq term+mux-session-host-color-alist
  '(("io"     . "#8fb28f")
    ("argo"   . "#8fb28f")
    ("phecda" . "#8fb28f")
    ("luxaky" . "#8c8ce8")
    ("nino"   . "#8c8ce8")))
(run-hook-with-args 'term+mux-new-session-hook term+mux-default-session t)
