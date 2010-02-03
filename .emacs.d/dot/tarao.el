(load "dot/util")
(load "dot/flymake")
(load "dot/cpp")
(load "dot/js")
(load "dot/perl")

;; no startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; mode-line
(defun viper-mode-string-p ()
  (or vimpulse-visual-mode
      (and (boundp 'viper-mode-string)
           viper-mode-string
           (not (eq viper-current-state 'vi-state)))))
(setq mode-line-frame-identification " ")
(setq default-mode-line-format
      '(""
        ; case without skk:
        ;   (normal) |-uuu:...
        ;   (insert) |--INSERT--uuu:...
        ; case with skk:
        ;   (normal) |--かな:uuu:...
        ;   (insert) |--INSERT--かな:uuu:...
        (:eval (and (viper-mode-string-p) "--"))
        (:eval (cond
                ((and vimpulse-visual-mode (eq vimpulse-visual-mode 'normal))
                 viper-visual-characterwise-state-id)
                ((and vimpulse-visual-mode (eq vimpulse-visual-mode 'line))
                 viper-visual-linewise-state-id)
                ((and vimpulse-visual-mode (eq vimpulse-visual-mode 'block))
                 viper-visual-blockwise-state-id)
                ((boundp 'viper-mode-string) viper-mode-string)
                (t "")))
        skk-modeline-input-mode
        (skk-mode
         ""
         ("-" (:eval (and (viper-mode-string-p) "-"))))
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
(setq skk-user-directory "~/.ddskk")
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
(setq viper-visual-characterwise-state-id "VISUAL")
(setq viper-visual-linewise-state-id "VLINE")
(setq viper-visual-blockwise-state-id "VBLOCK")
(setq viper-emacs-state-id "x")

;; viper-mode patches
(require 'hexl-viper-patch)
(defadvice viper-maybe-checkout (around viper-dont-ask-checkout activate) nil)

;; vimpulse
;(setq vimpulse-experimental nil) ; don't load bleeding edge code
(require 'vimpulse)
(setq woman-use-own-frame nil) ; don't create new frame for manpages

;; ;; vimpulse patches
(defvar in-viper-ex-visual nil)
(defadvice viper-ex (around ad-viper-ex activate)
  (setq in-viper-ex-visual vimpulse-visual-mode)
  ad-do-it
  (setq in-viper-ex-visual nil))
(defadvice viper-read-string-with-history
  (around viper-ex-visual (p &optional initial-str h d k m) activate)
  (let ((orig-str initial-str)
        (visual-addr "'<,'>")
        ret)
    (when (and in-viper-ex-visual initial-str)
      (setq initial-str visual-addr))
    (setq ret ad-do-it)
    (setq ad-return-value
          (if (and in-viper-ex-visual
                   (string-match (concat "\\`" visual-addr) ret))
              (replace-match orig-str nil t ret)
            ret))))

;; viper-mode keymaps
(define-key vimpulse-visual-mode-map
  (kbd ";") (lambda () (interactive) (viper-ex t)))
(define-key viper-vi-basic-map
  (kbd ";") 'viper-ex)
(define-key viper-vi-global-user-map
  (kbd ":") 'anything)
(define-key viper-vi-global-user-map
  (kbd "C-w") 'kill-region)
(define-key viper-insert-global-user-map
  (kbd "C-w") 'kill-region)
(define-key viper-vi-global-user-map
  (kbd "J") 'viper-scroll-up)
(define-key viper-vi-global-user-map
  (kbd "K") 'viper-scroll-down)
(define-key viper-insert-global-user-map
  (kbd "C-p") nil)
(define-key viper-insert-global-user-map
  (kbd "C-n") nil)

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

;; hatena
(add-hook 'find-file-hook
          '(lambda ()
             (if (string-match "/hatena/diary/" (buffer-file-name))
                 (progn
                   (make-variable-buffer-local 'make-backup-files)
                   (setq make-backup-files nil)
                   (auto-save-mode 0)))))
