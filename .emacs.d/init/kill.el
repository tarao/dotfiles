(eval-when-compile
  (require 'cl)
  (unless (fboundp 'cl-flet) (defalias 'cl-flet 'flet))
  (unless (fboundp 'cl-letf) (defalias 'cl-letf 'letf)))

(bundle browse-kill-ring
  (define-key global-map (kbd "M-y") #'browse-kill-ring))

(setq kill-do-not-save-duplicates t)

;; xsel command
(defvar x-available nil)
(defun x-available-p-1 ()
  (let ((display (or x-display-name (getenv "DISPLAY"))))
    (condition-case nil
        (and (stringp display) (> (length display) 0)
             (or (x-open-connection display)
                 ;; causes segfault in Emacs <= 24.3.50 + GTK3
                 (x-close-connection display)
                 t))
      (error nil))))
(defun x-available-p ()
  (let* ((frame (selected-frame))
         (x-available (frame-parameter frame 'x-available)))
    (unless x-available
      (setq x-available (if (x-available-p-1) 1 0))
      (set-frame-parameter frame 'x-available x-available))
    (eq x-available 1)))
(defsubst xsel-available-p () (and (executable-find "xsel") (x-available-p)))
(defun xsel-input (type text)
  (let* ((process-connection-type nil)
         (type (if (eq type 'PRIMARY) "-p" "-b"))
         (proc (start-process "xsel" nil "xsel" type "-i")))
    (process-send-string proc text)
    (process-send-eof proc)))
(defun xsel-output (type)
  (let ((type (if (eq type 'PRIMARY) "-p" "-b")))
    (with-temp-buffer
      (call-process "xsel" nil (current-buffer) nil type "-o")
      (buffer-substring-no-properties (point-min) (point-max)))))
(defun xsel-clear (type)
 (let ((type (if (eq type 'PRIMARY) "-p" "-b")))
   (call-process "xsel" nil 0 nil type "-c")))

;; Some applications recognize only PRIMARY section
;; See http://garin.jp/doc/Linux/xwindow_clipboard for details
(setq x-select-enable-primary t
      x-select-enable-clipboard t)

;; kill to X clipboard even if we are in terminal mode Emacs
(defadvice x-select-text (around ad-x-select-text (text &rest args) activate)
  (if (or (eq system-type 'windows-nt)
          (featurep 'ns)
          (eq (framep (selected-frame)) 'x)
          (not (xsel-available-p)))
      ad-do-it
    (cl-flet ((framep (frame) 'x)
              (x-set-cut-buffer (str &optional push) nil))
      (cl-letf (((symbol-function 'x-own-selection-internal)
                 (symbol-function 'xsel-input))
                ((symbol-function 'x-disown-selection-internal)
                 (symbol-function 'xsel-clear)))
        ad-do-it))))

;; yank from X clipboard even if we are in terminal mode Emacs
(defadvice x-selection-value (around ad-x-selection-value-xsel activate)
  (if (or (eq (framep (selected-frame)) 'x) (not (xsel-available-p)))
      ad-do-it
    (cl-flet ((framep (frame) 'x))
      (cl-letf (((symbol-function 'x-selection-value-internal)
                 (symbol-function 'xsel-output)))
        ad-do-it))))
