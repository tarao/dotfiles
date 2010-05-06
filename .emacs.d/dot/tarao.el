(load "dot/util")
(load "dot/flymake")
(load "dot/cpp")
(load "dot/js")
(load "dot/perl")
(load "dot/anything-zsh")
(load "dot/install")

;; no startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; mode-line
(setq mode-line-frame-identification " ")
(setq default-mode-line-format
      '(""
        mode-line-mule-info
        mode-line-modified
        mode-line-frame-identification
        mode-line-buffer-identification
        " "
        (-3 "%p")
        (line-number-mode
         (column-number-mode "(%l,%c)" " L%l")
         (column-number-mode " C%c"))
        " %[("
        mode-name
        mode-line-process
        minor-mode-alist
        "%n"
        ")%]-"
        (which-func-mode ("" which-func-format "-"))
;;         global-mode-string
        "-%-"))

(custom-set-faces
 '(minibuffer-prompt ((t (:foreground "blue"))))
 '(font-lock-builtin-face
   ((((class color) (min-colors 8)) (:foreground "brightblue"))))
 '(font-lock-comment-delimiter-face ((t (:inherit nil :foreground "green"))))
 '(font-lock-comment-face ((t (:foreground "green"))))
 '(font-lock-function-name-face ((t (:foreground "RoyalBlue"))))
 '(font-lock-keyword-face ((t (:foreground "blue" :weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "medium blue"))))
 '(font-lock-string-face ((t (:foreground "firebrick"))))
 '(font-lock-type-face ((t (:foreground "slate blue"))))
 '(font-lock-variable-name-face ((t (:foreground "BlueViolet"))))
 '(font-lock-warning-face ((t (:foreground "red"))))
 '(nxml-comment-content-face ((t (:foreground "DarkGreen"))))
 '(nxml-comment-delimiter-face ((t (:foreground "DarkGreen"))))
 '(nxml-delimited-data-face ((t (:foreground "firebrick"))))
 '(nxml-delimiter-face ((t (:foreground "medium blue"))))
 '(nxml-element-local-name-face
   ((t (:inherit nxml-name-face :foreground "dark blue"))))
 '(nxml-name-face ((t (:foreground "BlueViolet"))))
 '(nxml-tag-slash-face
   ((t (:inherit nxml-name-face :foreground "medium blue"))))
 '(trailing-whitespace ((t (:background "peach puff")))))

(defalias 'comm 'comment-region)
(defalias 'unc 'uncomment-region)

;; VC
(setq vc-follow-symlinks nil)
(setq vc-stay-local t)

;; hide-show
(defun hs-enable ()
  (interactive)
  (hs-minor-mode)
  (local-set-key (kbd "C-c h") 'hs-hide-block)
  (local-set-key (kbd "C-c s") 'hs-show-block)
  (local-set-key (kbd "C-c l") 'hs-hide-level))

;; Zenkaku -> Hankaku
(autoload 'zen2han-region "zen2han" "zen <=> han" t)
(autoload 'zen2han-buffer "zen2han" "zen <=> han" t)
(autoload 'zen2han-all-fill-paragraph-region "zen2han" "zen <=> han" t)
(autoload 'zen2han-all-fill-paragraph-buffer "zen2han" "zen <=> han" t)
(autoload 'zen2han-chop-line-end-space "zen2han" "zen <=> han" t)

;; SKK
(setq skk-user-directory "~/.ddskk")
(setq skk-use-viper t)
(setq-default mode-line-format
              (append '("" skk-modeline-input-mode) mode-line-format))

;; vimpulse
(load "dot/vimpulse")

;; line number mode
;; (setq linum-format
;;       (lambda (line)
;;         (let ((w (length (number-to-string
;;                           (count-lines (point-min) (point-max))))))
;;           (let ((fmt (concat "%" (number-to-string w) "d ")))
;;             (propertize (format fmt line) 'face 'linum)))))

;; key-chord
(require 'key-chord)
(setq key-chord-two-keys-delay 0.1)
(key-chord-mode 1)
(require 'space-chord)

;; synchronize kill buffer
(when (executable-find "xsel")
  (defun xsel-copy (start end)
    (let* ((process-connection-type nil)
           (proc (start-process "xsel" "*Messages*" "xsel" "-b" "-i")))
      (process-send-region proc start end)
      (process-send-eof proc)))
  (defadvice kill-region
    (before ad-xsel-kill-region (start end &optional yank-handler) activate)
    (xsel-copy start end))
  (defadvice copy-region-as-kill
    (before ad-xsel-copy-region-as-kill (start end) activate)
    (xsel-copy start end)))

;; auto-insert
(setq auto-insert-alist
      (append '(("\\.xhtml$" . ["insert.xhtml" my-template]))
              auto-insert-alist))

;; zen coding
(setq zencoding-preview-default nil) ; no preview
(setq zencoding-insert-flash-time 0.2)
;; auto-save
(defun auto-save-buffer (&optional buffer)
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (if (and buffer-file-name
         (buffer-modified-p)
             (not buffer-read-only)
             (file-writable-p buffer-file-name))
        (save-buffer))))

;; hatena
(load "multi-mode-util" t)
(setq hatena-diary-super-pre-languages '(java javascript lisp ruby))
(defun hatena-diary-super-pre-notation ()
  (interactive)
  (multi-mode-init 'text-mode)
  (dolist (l hatena-diary-super-pre-languages)
    (let ((str (symbol-name l)))
      (multi-install-chunk-finder (concat "^>|" str "|$") "^||<$"
                                  (intern (concat str "-mode"))))))
(add-hook 'find-file-hook
          '(lambda ()
             (when (string-match "/hatena/diary/" (buffer-file-name))
               (set (make-variable-buffer-local 'make-backup-files) nil)
               (auto-save-mode 0)
               (when (featurep 'multi-mode-util)
                 (add-hook
                  'multi-indirect-buffer-hook
                  '(lambda ()
                     (set (make-variable-buffer-local 'make-backup-files) nil)
                     (auto-save-mode 0)))
                 (hatena-diary-super-pre-notation)))))
