(bundle term+)
(with-eval-after-load-feature (term term+)
  (require 'xterm-256color)
  (let ((vec [unspecified
              "black"   "#aa0000" "#00aa00" "#aa5500"
              "#8c8ce8" "#aa00aa" "#00aaaa" "#aaaaaa"
              "#555555" "#ff5555" "#55ff55" "#ffff55"
              "#5555ff" "#ff55ff" "#55ffff" "white"]))
    (if (string< "24.2" emacs-version)
        (dotimes (i 16)
          (let ((face (elt ansi-term-color-vector (1+ i))))
            (set-face-foreground face (elt vec (1+ i)))
            (set-face-background face (elt vec (1+ i)))))
      (setq ansi-term-color-vector vec)))
  (bundle! term+key-intercept)
  (bundle! term+mode)
  (with-eval-after-load-feature 'evil (bundle! term+evil)))

(bundle term+mux
  (with-eval-after-load-feature 'term+mux
    (define-key term+char-map (kbd "C-z") term+mux-map)
    (setq term+mux-session-host-color-alist
          '(("io"     . "#8fb28f")
            ("argo"   . "#8fb28f")
            ("phecda" . "#8fb28f")
            ("luxaky" . "#8c8ce8")
            ("nino"   . "#8c8ce8")))
    (run-hook-with-args 'term+mux-new-session-hook
                        term+mux-default-session t)))
