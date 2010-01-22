(load "dot/util")
(load "dot/flymake")
(load "dot/cpp")
(load "dot/js")
(load "dot/perl")

;; no startup message
(setq inhibit-startup-message t)

;; mode-line
(setq mode-line-frame-identification " ")
(setq default-mode-line-format
      '(""
        ; case without skk:
        ;   (normal) |-uuu:...
        ;   (insert) |--INSERT--uuu:...
        ; case with skk:
        ;   (normal) |--かな:uuu:...
        ;   (insert) |--INSERT--かな:uuu:...
        (viper-mode-string
         (:eval (unless (eq viper-current-state 'vi-state) "--")))
        viper-mode-string
        skk-modeline-input-mode
        (skk-mode
         ""
         ("-" (viper-mode-string
               (:eval (unless (string= viper-mode-string "") "-")))))
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
(setq skk-use-viper t)

;; viper minor mode
(setq viper-custom-file-name (locate-library "dot/.viper"))
(setq viper-mode t)
(setq viper-toggle-key (kbd "C-x C-z"))
(setq viper-ex-style-motion nil)
(setq viper-ex-style-editing nil)
(setq blink-matching-paren t)
(require 'viper)
(setq viper-vi-state-id "")
(setq viper-insert-state-id "INSERT")
(setq viper-replace-state-id "REPLACE")
(setq viper-visual-characterwise-state-id "VISUAL")
(setq viper-visual-linewise-state-id "VLINE")
(setq viper-visual-blockwise-state-id "VBLOCK")
(setq viper-emacs-state-id "x")

;; viper-mode patches
(require 'hexl-viper-patch)
(defadvice viper-maybe-checkout (around viper-dont-ask-checkout activate) nil)
(defadvice ac-start (around viper-replace-no-ac-start (&optional msg) activate)
  (unless (eq viper-current-state 'replace-state) ad-do-it))

;; line number mode
;; (setq linum-format
;;       (lambda (line)
;;         (let ((w (length (number-to-string
;;                           (count-lines (point-min) (point-max))))))
;;           (let ((fmt (concat "%" (number-to-string w) "d ")))
;;             (propertize (format fmt line) 'face 'linum)))))

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
