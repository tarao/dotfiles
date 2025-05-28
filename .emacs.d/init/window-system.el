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
         (width . ,line-column)
         (foreground-color . ,(face-foreground 'default))
         (background-color . ,(face-background 'default)))))
  (setq-default initial-frame-alist (append frame-alist initial-frame-alist)
                default-frame-alist (append frame-alist default-frame-alist)))

;; utility
(defun after-frame-update (function &rest args)
  (apply 'run-with-idle-timer 0 nil function args))

;; creating new frames
(defun clone-frame-1 (direction)
  (let* ((frame (selected-frame))
         (left (eval (frame-parameter frame 'left)))
         (top (eval (frame-parameter frame 'top)))
         (width (frame-width frame))
         (height (frame-height frame))
         (pixel-width (frame-pixel-width frame))
         (display-width (x-display-pixel-width))
         (x-offset 0) (y-offset 0))
    (make-frame
     `((left . ,(+ x-offset
                   (min (- display-width pixel-width)
                        (max 0 (+ left (* direction pixel-width))))))
       (top . ,(+ y-offset top))
       (width . ,width)
       (height . ,height)))))
(defun clone-frame-to-left ()
  "Create a new frame in the same size as the current frame and
place the new frame at the left side of the current frame."
  (interactive)
  (if (display-graphic-p)
      (clone-frame-1 -1)
    (select-frame (make-frame))))
(defun clone-frame-to-right ()
  "Create a new frame in the same size as the current frame and
place the new frame at the right side of the current frame."
  (interactive)
  (if (display-graphic-p)
      (clone-frame-1 1)
    (select-frame (make-frame))))

;; side window

(defvar inhibit-frame-expansion nil)

(defun enlarge-frame (direction width)
  (when (and window-system (not inhibit-frame-expansion))
    (let* ((frame (selected-frame))
           (history (frame-parameter frame 'enlarged))
           (last (cdr (assq direction history))))
      (setq history (cons (cons direction width) (assq-delete-all direction history)))
      (set-frame-parameter frame 'enlarged history)
      (let* ((frame-width (frame-width frame))
             (other-width (/ (+ (apply '+ (mapcar '(lambda (x) (or x 0)) (window-fringes)))
                                (window-scroll-bar-width))
                             (frame-char-width frame)))
             (delta (if last (- width last) (+ width other-width)))
             (pos (frame-position frame))
             (left (car pos))
             (top (cdr pos))
             (pixel-width (frame-outer-width frame)))
        (unless (= delta 0)
          (set-frame-width frame (+ delta frame-width))
          (when (eq direction 'left)
            (after-frame-update
             '(lambda (frame pixel-width left top)
                (let* ((new-pixel-width (frame-outer-width frame))
                       (pixel-delta (- new-pixel-width pixel-width)))
                  (my:set-frame-position frame (max 0 (- left pixel-delta)) top)))
             frame pixel-width left top)))))))

(defun shrink-frame (direction)
  (when (and window-system (not inhibit-frame-expansion))
    (let* ((frame (selected-frame))
           (frame-width (frame-width frame))
           (other-width (/ (+ (apply '+ (mapcar '(lambda (x) (or x 0)) (window-fringes)))
                              (window-scroll-bar-width))
                           (frame-char-width frame)))
           (pos (frame-position frame))
           (left (car pos))
           (top (cdr pos))
           (pixel-width (frame-outer-width frame))
           (history (frame-parameter frame 'enlarged))
           (enlarged (assq direction history)))
      (when enlarged
        (setq history (assq-delete-all (car enlarged) history))
        (set-frame-parameter frame 'enlarged history)
        (set-frame-width frame (- frame-width other-width (cdr enlarged)))
        (when (eq direction 'left)
          (run-with-idle-timer ; wait for frame width update
           0 nil
           '(lambda (frame pixel-width left top)
              (let* ((new-pixel-width (frame-outer-width frame))
                     (pixel-delta (- pixel-width new-pixel-width)))
                (my:set-frame-position frame (+ left pixel-delta) top)))
           frame pixel-width left top))))))

(defun enlarge-frame-for-side-window (orig-fun buffer alist)
  (let ((win (funcall orig-fun buffer alist))
        (side (cdr (assq 'side alist)))
        (width (or (cdr (assq 'window-width alist)) sidebar-width)))
    (when (and win (or (eq side 'left) (eq side 'right)))
      (when (floatp width)
        (setq width (truncate (* (window-width win) width))))
      (with-selected-window win
        (unless (memq side (frame-parameter (selected-frame) 'window-sides))
          (enlarge-frame side width))))
    win))
(advice-add 'display-buffer-in-side-window :around #'enlarge-frame-for-side-window)

(defun maybe-shrink-frame-for-side-window ()
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (let* ((windows (window-list frame))
             (orig-sides (frame-parameter frame 'window-sides))
             sides)
        (dolist (win windows)
          (let ((side (window-parameter win 'window-side)))
            (when side
              (push side sides))))
        (when (and (memq 'left orig-sides) (not (memq 'left sides)))
          (shrink-frame 'left))
        (when (and (memq 'right orig-sides) (not (memq 'right sides)))
          (shrink-frame 'right))
        (set-frame-parameter frame 'window-sides sides)))))
(add-hook 'window-configuration-change-hook #'maybe-shrink-frame-for-side-window)

;; frame-position workaround

(defvar my:frame-offset-left 0)
(defvar my:frame-offset-top 0)

(defun my:set-frame-position (frame x y)
  "Set position of FRAME to (X, Y) as a reverse function of `frame-position'.

In other words,

 (let ((pos (frame-position)))
   (my:set-frame-position nil (car pos) (cdr pos)))

doesn't move the frame position.

On the contrary, the original `set-frame-position' moves the
position when you specify the value returned from
`frame-position'.  See

https://debbugs.gnu.org/cgi/bugreport.cgi?bug=38452

for the detail."
  (set-frame-position frame
                      (if (< x 0) x (+ x my:frame-offset-left))
                      (if (< y 0) y (+ y my:frame-offset-top))))

(defun my:adjust-frame-offset (frame &optional callback)
  ;; move to top left and calculate frame position offset
  (let* ((workarea (cdr (assq 'workarea (car (display-monitor-attributes-list)))))
         (left (nth 0 workarea))
         (top (nth 1 workarea)))
    (set-frame-position frame left top)
    (sit-for 0.1)
    (setq my:frame-offset-left (- left (eval (frame-parameter frame 'left)))
          my:frame-offset-top (- top (eval (frame-parameter frame 'top))))
    (when callback (funcall callback))))

(defcustom desktop-offset-left 0
  "Left offst of the desktop in pixels"
  :type 'number
  :group 'frames)
(defcustom desktop-offset-top 0
  "Top offst of the desktop in pixels"
  :type 'number
  :group 'frames)
(defcustom desktop-offset-right 0
  "Right offst of the desktop in pixels"
  :type 'number
  :group 'frames)
(defcustom desktop-offset-bottom 0
  "Bottom offst of the desktop in pixels"
  :type 'number
  :group 'frames)
(defun screen-size ()
  (let ((screen-width 0) (screen-height 0))
    (dolist (attrs (display-monitor-attributes-list))
      (let* ((geometry (cdr (assq 'geometry attrs)))
             (right (+ (nth 0 geometry) (nth 2 geometry)))
             (bottom (+ (nth 1 geometry) (nth 3 geometry))))
        (when (> right screen-width) (setq screen-width right))
        (when (> bottom screen-height) (setq screen-height bottom))))
    (list screen-width screen-height)))
(defun display-dimensions (selector)
  (eval-and-compile (require 'cl-lib))
  (let ((largest-display-size 0)
        (frame (selected-frame))
        largest dimensions)
    (cl-loop for attrs in (display-monitor-attributes-list)
             for geometry = (cdr (assq 'geometry attrs))
             for workarea = (cdr (assq 'workarea attrs))
             for left = (nth 0 workarea)
             for top = (nth 1 workarea)
             for width = (nth 2 workarea)
             for height = (nth 3 workarea)
             for display-size = (* (nth 2 geometry) (nth 3 geometry))
             for right = (+ left width)
             for bottom = (+ top height)
             do (setq dimensions (list left top right bottom width height))
             when (> display-size largest-display-size)
             do (setq largest dimensions
                      largest-display-size display-size)
             until (or (eq selector 'first)
                       (and (eq selector 'current)
                            (memq frame (cdr (assq 'frames attrs))))))
    (if (eq selector 'largest)
        largest
      dimensions)))
(defun display-frame-position (selector direction)
  (let ((frame (selected-frame))
        (dimensions (display-dimensions selector))
        (screen (screen-size)))
    (when dimensions
      (let* ((frame-width (frame-pixel-width frame))
             (left (if (eq direction 'left)
                       (max desktop-offset-left
                            (nth 0 dimensions))
                     (min (- (nth 0 screen) desktop-offset-right frame-width)
                          (- (nth 2 dimensions) frame-width))))
             (top (max desktop-offset-top (nth 1 dimensions)))
             (height (/ (- (nth 5 dimensions)
                           desktop-offset-top desktop-offset-bottom)
                        (frame-char-height frame))))
        (list left top height)))))
(defun fit-frame-to-display (selector direction)
  (let ((pos (display-frame-position selector direction))
        (frame (selected-frame)))
    (my:set-frame-position frame (nth 0 pos) (nth 1 pos))
    (set-frame-height frame (nth 2 pos))
    pos))
(defun fit-frame-to-largest-display-left ()
  "Fit the current frame to the left end of the largest display."
  (interactive)
  (fit-frame-to-display 'largest 'left))
(defun fit-frame-to-largest-display-right ()
  "Fit the current frame to the right end of the largest display."
  (interactive)
  (fit-frame-to-display 'largest 'right))
(defun fit-frame-to-first-display-left ()
  "Fit the current frame to the left end of the first display."
  (interactive)
  (fit-frame-to-display 'first 'left))
(defun fit-frame-to-first-display-right ()
  "Fit the current frame to the right end of the first display."
  (interactive)
  (fit-frame-to-display 'first 'right))

(defun wayland-p ()
  (string= (getenv "XDG_SESSION_TYPE") "wayland"))

(defun gnome-p ()
  (string-match-p "\\bgnome\\b" (downcase (or (getenv "XDG_CURRENT_DESKTOP") ""))))

(defconst window-commander-command-prefix
  '("gdbus" "call" "--session" "--dest" "org.gnome.Shell"
    "--object-path" "/org/gnome/Shell/Extensions/WindowCommander"
    "--method"))

(defun call-window-commander (method &rest args)
  (let* ((command (mapconcat (lambda (s) (format "%s" s))
                             (append window-commander-command-prefix
                                     (cons (concat "org.gnome.Shell.Extensions.WindowCommander." method) args))
                             " "))
         (result (shell-command-to-string command)))
    (and result
         (string-trim-right (string-trim-left result "('") "',)\n"))))

(defun set-frame-position-size-wayland-gnome (left top width height)
  (eval-and-compile (require 'cl-lib))
  (unless (and (executable-find "gnome-extensions")
               (= 0 (call-process-shell-command "gnome-extensions info window-commander@gnikolaos.gr"))
               (executable-find "gdbus"))
    (error "You need to have gnome-extension and gdbus commands and window-commander \
extension installed to move frame under Wayland GNOME environment"))
  (let* ((windows (json-parse-string (call-window-commander "List") :object-type 'plist :array-type 'list :false-object nil))
         (window-id (cl-loop for window in windows
                             when (and (plist-get window :in_current_workspace)
                                       (plist-get window :focus))
                             return (plist-get window :id))))
    (call-window-commander "Place" window-id left top width height)))

(defun tile-frame-horizontally (splits index)
  "Place the current frame at INDEX in horizontal SPLITS of screen"
  (let* ((frame (selected-frame))
         (dimensions (display-dimensions 'current))
         (screen-left (nth 0 dimensions))
         (screen-top (nth 1 dimensions))
         (screen-width (nth 4 dimensions))
         (screen-height (nth 5 dimensions))
         (width (floor (/ screen-width splits)))
         (left (+ screen-left (* width (1- index))))
         (top (max desktop-offset-top screen-top))
         (width (- width desktop-offset-left desktop-offset-right))
         (height (- screen-height desktop-offset-top desktop-offset-bottom)))
    (if (and (wayland-p) (gnome-p))
        (set-frame-position-size-wayland-gnome left top width height)
      (when (or (> width screen-width) (> height screen-height))
        (set-frame-size frame (min width screen-width) (min height screen-height) t))
      (my:set-frame-position frame screen-left screen-top)
      (set-frame-size frame width height t)
      (my:set-frame-position frame left top))))

(defun tile-windows-horizontally (n)
  "Maximize current frame and split window horizontally"
  (interactive "P")
  (set-frame-parameter nil 'fullscreen 'maximized)
  (delete-other-windows (selected-window))
  (call-interactively 'split-window-horizontally-n))

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

(when window-system
  (if after-init-time
      ;; already initialized
      (setup-window-system-configuration)
    (add-hook 'after-init-hook #'setup-window-system-configuration)))
(add-hook 'after-make-frame-functions #'setup-window-system-configuration)
