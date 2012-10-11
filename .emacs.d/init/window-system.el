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

(defvar default-font nil)

(defun setup-window-system-configuration (&optional frame)
  "Initialize configurations for window system.
Configurations, which require X (there exists a frame), are
placed in this function.

When Emacs is started as a GUI application, just running this
function initializes the configurations.

When Emacs is started as a daemon, this function should be called
just after the first frame is created by a client.  For this,
this function is added to `after-make-frame-functions' and
removed from them after the first call."
  (with-selected-frame (or frame (selected-frame))
    (when window-system
      ;; default font
      (create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal"
                                      nil "menloja")
      (set-fontset-font "fontset-menloja"
                        'unicode
                        (font-spec :family "Hiragino Kaku Gothic ProN")
                        nil
                        'append)
      (setq face-font-rescale-alist '(("Hiragino.*" . 1.2)))
      (setq default-font "fontset-menloja")
      (set-default-font default-font)
      (setq-default initial-frame-alist
                    (append `((font . ,default-font)) initial-frame-alist))
      (setq-default default-frame-alist
                    (append `((font . ,default-font)) default-frame-alist))
      ;; current frame
      (set-frame-parameter (selected-frame) 'font default-font)
      ;; call once
      (remove-hook 'after-make-frame-functions
                   #'setup-window-system-configuration))))

(if window-system
    (setup-window-system-configuration)
  (add-hook 'after-make-frame-functions #'setup-window-system-configuration))
