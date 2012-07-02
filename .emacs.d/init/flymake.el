(require 'flymake)

;; timeout
(setq flymake-no-change-timeout 3)

;; faces
(set-face-background 'flymake-errline "red4")
(set-face-foreground 'flymake-errline "black")
(set-face-background 'flymake-warnline "yellow")
(set-face-foreground 'flymake-warnline "black")

;; show errors on minibuffer
;; http://d.hatena.ne.jp/xcezx/20080314/1205475020
(defun flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list
          (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count
          (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file
                (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file
                (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line
                (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

;; automatically show errors on minibuffer
(defadvice flymake-post-syntax-check
  (after flymake-display-err-minibuf-auto last activate)
  (flymake-display-err-minibuf))

;; ignore when visiting non-file buffer
(defadvice flymake-get-init-function
  (around flymake-ignore-non-file (file-name) activate)
  (if file-name ad-do-it '(lambda () nil)))
