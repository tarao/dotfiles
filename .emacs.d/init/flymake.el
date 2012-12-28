(bundle tarao-elisp)

;; timeout
(setq-default flymake-no-changes-timeout 0.7)

(eval-after-load 'flymake
  '(progn
     ;; show errors on minibuffer
     (require 'flymake-minibuffer)

     ;; faces
     (set-face-background 'flymake-errline "IndianRed1")
     (set-face-foreground 'flymake-errline "#444444")
     (set-face-background 'flymake-warnline "LightGoldenrod1")
     (set-face-foreground 'flymake-warnline "#444444")))
