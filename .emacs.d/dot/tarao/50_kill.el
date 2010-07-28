;; synchronize kill buffer
(when (and (executable-find "xsel") (getenv "DISPLAY"))
  (defun xsel-copy (start end)
    (let* ((process-connection-type nil)
           (proc (start-process "xsel" "*Messages*" "xsel" "-b" "-i")))
      (process-send-region proc start end)
      (process-send-eof proc)))
  (defadvice kill-region
    (before ad-xsel-kill-region (start end &optional yank-handler) activate)
    (xsel-copy start end))
  (defadvice copy-region-as-kill
    (before ad-xsel-copy-region-as-kill (start end) activate)
    (xsel-copy start end)))
