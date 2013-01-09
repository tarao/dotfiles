(bundle pit)
(bundle hatena-markup-mode)
(bundle hatena-multi-mode
  (setq-default hatena:mm:filetype-alist '((ocaml . tuareg))))
(bundle hatena-diary
  (setq-default hatena:d:major-mode 'hatena:markup-mode))

(eval-after-load-compile 'hatena-diary
  ;; load account information via pit
  (let* ((spec '((username . "")
                 (password . "")))
         (config (pit/get 'hatena `(require ,spec))) (sym "hatena:%s"))
    (mapc #'(lambda (k) (set (intern (format sym k)) (cdr (assq k config))))
          (mapcar #'car spec)))

  ;; multi-mode
  (add-hook 'hatena:markup-mode-hook #'hatena:multi-mode))
