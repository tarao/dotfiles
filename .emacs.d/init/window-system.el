;; cursor
(blink-cursor-mode 0)
(setq cursor-type 'box)
(defvar default-cursor-color "white")
(set-cursor-color default-cursor-color)

;; line space
(setq-default line-spacing 0)

;; do not use dialog box
(setq use-dialog-box nil)
(defun message-box (str &rest args)
  "Display a message at the bottom of the screen."
  (apply #'message str args))

;; initial frame parameters
(let ((frame-alist
       `((cursor-color . ,default-cursor-color)
         (mouse-color . ,default-cursor-color)
         (vertical-scroll-bars . nil)
         (width . 80)
         (foreground-color . "#aaaaaa")
         (background-color . "#1f1f1f"))))
  (setq-default initial-frame-alist (append frame-alist initial-frame-alist)
                default-frame-alist (append frame-alist default-frame-alist)))

;; font settings
(defconst default-fontset-name "menloja")
(defconst default-base-font-name "Menlo")
(defconst default-base-font-size 10)
(defconst default-ja-font-name "Hiragino Kaku Gothic ProN")
(defconst default-ja-font-pat "Hiragino.*")
(defconst default-ja-font-scale 1.3)

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
      (let* ((fontset-name default-fontset-name)
             (base default-base-font-name) (size default-base-font-size)
             (ja default-ja-font-name) (ja-pat default-ja-font-pat)
             (scale default-ja-font-scale)
             (base-font (format "%s-%d:weight=normal:slant=normal" base size))
             (ja-font (font-spec :family ja))
             (fsn (concat "fontset-" fontset-name))
             (elt (list (cons 'font fsn))))
        ;; create font
        (create-fontset-from-ascii-font base-font nil fontset-name)
        (set-fontset-font fsn 'unicode ja-font nil 'append)
        (add-to-list 'face-font-rescale-alist (cons ja-pat scale))
        ;; default
        (set-frame-font fsn)
        (setq-default initial-frame-alist (append elt initial-frame-alist)
                      default-frame-alist (append elt default-frame-alist))
        ;; current frame
        (set-frame-parameter (selected-frame) 'font fsn)
        ;; call once
        (remove-hook 'after-init-hook #'setup-window-system-configuration)
        (remove-hook 'after-make-frame-functions
                     #'setup-window-system-configuration)))))

(if window-system
    (add-hook 'after-init-hook #'setup-window-system-configuration)
  (add-hook 'after-make-frame-functions #'setup-window-system-configuration))

(defun close-frame-display (frame)
  "Close FRAME's X connection."
  (let* ((get-display #'(lambda (f)
                          (and (eq (framep f) 'x)
                               (terminal-name (frame-terminal f)))))
         (display (funcall get-display frame))
         (frames (remq frame (frame-list)))
         (displays (and display (mapcar get-display frames)))
         (hook (and (boundp 'delete-frame-functions)
                    (memq 'close-frame-display delete-frame-functions))))
    (when (and display (not (member display displays)))
      (remove-hook 'delete-frame-functions #'close-frame-display)
      (delete-frame frame)
      (x-close-connection display) ; causes segfault in Emacs <= 24.3.50 + GTK3
      (when hook (add-hook 'delete-frame-functions #'close-frame-display)))))
;; close frame display when the frame is deleted (we need this to
;; ensure that an emacs daemon without X window has no X connection)
(add-hook 'delete-frame-functions #'close-frame-display)
