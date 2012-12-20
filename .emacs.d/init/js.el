(setq auto-mode-alist
      (append '(("\\.json$" . js-mode)
                ("\\.js$" . js-mode))
              auto-mode-alist)
      js-indent-level 4
      js-expr-indent-offset 4)

(defconst flymake-allowed-js-file-name-masks
  '(("\\.json$" flymake-js-init)
    ("\\.js$" flymake-js-init)))
(defcustom flymake-js-detect-trailing-comma t nil :type 'boolean)
(defvar flymake-js-err-line-patterns
  '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" 1 2 nil 3)))
(when flymake-js-detect-trailing-comma
  (setq
   flymake-js-err-line-patterns
   (append
    flymake-js-err-line-patterns
    '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(strict warning: trailing comma.+\\)\:$"
       1 2 nil 3)))))

(defvar flymake-js-e4x-enabled nil)
(defvar flymake-js-use-strict t)
(defun flymake-js-command-opt (ls)
  (let ((base (cons "-C" ls)))
    (let ((strct (if flymake-js-use-strict (cons "-s" base) base)))
      (if flymake-js-e4x-enabled (cons "-x" strct) strct))))
(defun flymake-js-e4x ()
  (interactive)
  (setq flymake-js-e4x-enabled (not flymake-js-e4x-enabled)))
(defun flymake-js-strict ()
  (interactive)
  (setq flymake-js-use-strict (not flymake-js-use-strict)))

(defun flymake-js-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "js" (flymake-js-command-opt (list local-file)))))
(defun flymake-js-load ()
  (interactive)
  ;; (defadvice flymake-post-syntax-check
  ;;   (before flymake-force-check-was-interrupted)
  ;;   (setq flymake-check-was-interrupted t))
  ;; (ad-activate 'flymake-post-syntax-check)
  (set (make-local-variable 'flymake-err-line-patterns)
       flymake-js-err-line-patterns)
  (flymake-mode t))
(defadvice flymake-post-syntax-check
  (before flymake-force-check-was-interrupted activate)
  (setq flymake-check-was-interrupted t))

(setq flymake-allowed-file-name-masks
      (append flymake-allowed-file-name-masks
              flymake-allowed-js-file-name-masks))

(add-hook 'js-mode-hook
          '(lambda ()
             (progn
               (hs-enable)
               (flymake-js-load))))
