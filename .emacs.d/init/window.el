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

(defun split-window-horizontally-n (n)
  "Split window horizontally by N"
  (interactive "P")
  (unless (integerp n)
    (setq n (read-number "Splits: ")))
  (unless (and (integerp n) (> n 1))
    (error "Splits must be greater than 1"))
  (let ((width (floor (/ (window-width) n)))
        (window (selected-window))
        (i 1))
    (while (< i n)
      (setq window (split-window window width t)
            i (1+ i)))
    (balance-windows)))

;; sidebar
(defvar sidebar-width 35)
(defun adjust-side-window-width (orig-fun buffer alist)
  (let* ((win (funcall orig-fun buffer alist))
         (side (cdr (assq 'side alist)))
         (width (cdr (assq 'window-width alist))))
    (when (and win (or (eq side 'left) (eq side 'right)))
      (with-selected-window win
        (when (floatp width)
          (setq width (truncate (* (window-width win) width))))
        (unless width
          (setq width sidebar-width))
        (let ((current-width (window-width win))
              (window-size-fixed))
          (unless (= width current-width)
            (window-resize win (- width current-width) t)))))
    win))
(advice-add 'display-buffer-in-side-window :around 'adjust-side-window-width)
