(eval-when-compile (require 'cl))

(defconst js-mode-files '("\\.js$" "\\.json$"))

;; auto-mode
(setq auto-mode-alist
      (append (mapcar #'(lambda (x) (cons x 'js-mode)) js-mode-files)
              auto-mode-alist))

;; syntax
(setq-default js-indent-level 4
              js-expr-indent-offset 4)

;; flymake

(defvar flymake-jshint-command "jshint")
(defvar flymake-jshint-command-args nil)
(defvar flymake-jshint-config nil)
(defconst flymake-js-err-line-patterns
  '((".*: line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.*\\)$"
     nil 1 2 3)))
(defconst flymake-allowed-js-file-name-masks
  (mapcar #'(lambda (x) (list x 'flymake-js-init)) js-mode-files))

(autoload 'flymake-init-create-temp-buffer-copy "flymake")
(defun flymake-js-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name)))
         (args (list* local-file flymake-jshint-command-args)))
    (when flymake-jshint-config
      (let ((config (expand-file-name flymake-jshint-config)))
        (setq args (list* "--config" config args))))
    (list flymake-jshint-command args)))

(with-eval-after-load-feature 'flymake
  (setq flymake-allowed-file-name-masks
        (append flymake-allowed-file-name-masks
                flymake-allowed-js-file-name-masks)))

(add-hook 'js-mode-hook
          #'(lambda ()
             (hs-enable)
             (flymake-mode 1)
             (when (boundp 'flymake-err-line-patterns)
               (set (make-local-variable 'flymake-err-line-patterns)
                    flymake-js-err-line-patterns))))
