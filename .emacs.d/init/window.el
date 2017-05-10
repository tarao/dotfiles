;; window selection with numbers
(bundle switch-window)

;; window selection by directional keys
(setq-default windmove-wrap-around t)
(windmove-default-keybindings)

(winner-mode t)

(bundle! popwin
  (add-to-list 'display-buffer-alist
               '("." nil (reusable-frames . t)))
  (setq popwin:reuse-window t)
  (popwin-mode 1))
