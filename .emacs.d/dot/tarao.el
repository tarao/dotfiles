(load "dot/util")
(load "dot/flymake")
(load "dot/cpp")
(load "dot/js")
(load "dot/perl")
(load "dot/install")

;; no startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; mode-line
(defun viper-mode-string-p ()
  (condition-case ()
      (or vimpulse-visual-mode (not (eq viper-current-state 'vi-state)))
    (error nil)))
(setq viper-my-mode-string "--x--")
(setq viper-after-mode-string "")
(defun viper-update-my-mode-string ()
  (make-variable-buffer-local 'viper-my-mode-string)
  (make-variable-buffer-local 'viper-after-mode-string)
  (condition-case ()
      (cond
       ((viper-mode-string-p)
        (setq viper-my-mode-string
              (cond
               ((eq vimpulse-visual-mode 'normal)
                viper-visual-characterwise-state-id)
               ((eq vimpulse-visual-mode 'line)
                viper-visual-linewise-state-id)
               ((eq vimpulse-visual-mode 'block)
                viper-visual-blockwise-state-id)
               ((eq viper-current-state 'insert-state)
                viper-insert-state-id)
               ((eq viper-current-state 'replace-state)
                viper-replace-state-id)
               ((eq viper-current-state 'emacs-state)
                viper-emacs-state-id)
               (t "")))
        (setq viper-my-mode-string (concat "--" viper-my-mode-string))
        (setq viper-after-mode-string "--"))
       (t
        (setq viper-my-mode-string "")
        (setq viper-after-mode-string "-")))
    (error nil)))
(defadvice viper-change-state (after ad-viper-update-my-mode-string activate)
  (viper-update-my-mode-string))
(setq mode-line-frame-identification " ")
(setq default-mode-line-format
      '(""
        ; case without skk:
        ;   (normal) |-uuu:...
        ;   (insert) |--INSERT--uuu:...
        ; case with skk:
        ;   (normal) |--かな:uuu:...
        ;   (insert) |--INSERT--かな:uuu:...
        viper-my-mode-string
        skk-modeline-input-mode
        (skk-mode
         ""
         viper-after-mode-string)
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
(setq jaspace-mode-string " WS")
(setq undo-tree-mode-lighter nil)

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
