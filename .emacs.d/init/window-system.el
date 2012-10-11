;; cursor
(blink-cursor-mode 0)
(setq cursor-type 'box)
(setq default-cursor-color "white")
(set-cursor-color default-cursor-color)

;; line space
(setq-default line-spacing 0)

;; do not use dialog box
(setq use-dialog-box nil)

;; initial frame parameters
(let ((frame-alist
       `((cursor-color . ,default-cursor-color)
         (vertical-scroll-bars . nil)
         (foreground-color . "#aaaaaa")
         (background-color . "#1f1f1f"))))
  (setq-default initial-frame-alist
                (append frame-alist initial-frame-alist))
  (setq-default default-frame-alist
                (append frame-alist default-frame-alist)))

(defvar window-system-configured nil)
(defun setup-window-system-configuration ()
  (when (and window-system (not window-system-configured))
    ;; font
    (create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal"
                                    nil "menloja")
    (set-fontset-font "fontset-menloja"
                      'unicode
                      (font-spec :family "Hiragino Kaku Gothic ProN"
                                 :size 16)
                      nil
                      'append)
    (setq default-font "fontset-menloja")
    (set-default-font default-font)
    (setq-default initial-frame-alist
                  (append `((font . ,default-font)) initial-frame-alist))
    (setq-default default-frame-alist
                  (append `((font . ,default-font)) default-frame-alist))
    (setq window-system-configured t)))

(if window-system
    (setup-window-system-configuration)
  (add-hook 'after-make-frame-functions
            '(lambda (f)
               (with-selected-frame f
                 (setup-window-system-configuration)))))
