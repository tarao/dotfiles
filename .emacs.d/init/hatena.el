(bundle pit)
(bundle hatena-markup-mode)
(bundle hatena-diary
  (setq-default hatena:d:major-mode 'hatena-markup-mode))

(eval-after-load-compile 'hatena-diary
  ;; load account information via pit
  (let* ((spec '((username . "Hatena user name")
                 (password . "Hatena password")))
         (config (pit/get 'hatena `(require ,spec))) (sym "hatena:%s"))
    (mapc #'(lambda (k) (set (intern (format sym k)) (cdr (assq k config))))
          (mapcar #'car spec))))
