(load "multi-mode-util" t)
(defun hatena-diary-init ()
  (interactive)
  (no-backup)
  (when (featurep 'multi-mode-util)
    (add-hook
     'multi-indirect-buffer-hook
     '(lambda () (no-backup)))
    (hatena-diary-install-multi-mode)))
(add-hook 'find-file-hook
          '(lambda ()
             (when (string-match "/hatena/diary/" (buffer-file-name))
               (hatena-diary-init))))
