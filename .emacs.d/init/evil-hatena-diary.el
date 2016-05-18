(bundle evil
  ;; hatena-diary
  (push 'hatena:d:list-mode evil-motion-state-modes)
  (with-eval-after-load-feature (evil hatena-diary)
    (evil-make-overriding-map hatena:d:list-mode-map)
    (evil-add-hjkl-bindings hatena:d:list-mode-map 'motion)))
