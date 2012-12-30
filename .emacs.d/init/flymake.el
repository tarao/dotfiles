(bundle tarao-elisp)

;; timeout
(setq-default flymake-no-changes-timeout 0.7)

(defadvice flymake-post-syntax-check
  (before flymake-force-check-was-interrupted activate)
  (setq flymake-check-was-interrupted t))

(eval-after-load-compile 'flymake
  ;; show errors on minibuffer
  (require 'flymake-minibuffer)

  ;; faces
  (set-face-background 'flymake-errline "IndianRed1")
  (set-face-foreground 'flymake-errline "#444444")
  (set-face-background 'flymake-warnline "LightGoldenrod1")
  (set-face-foreground 'flymake-warnline "#444444"))
