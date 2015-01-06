(defvar flymake-c-command "/usr/bin/gcc")
(defvar flymake-cc-command "/usr/bin/g++")
(defvar flymake-cc-command-args
  '("-fsyntax-only"
    ;; "-std=c++98" "-pedantic-errors"
    "-Wall" "-Wextra"
    ;; "-Wcast-qual" "-Wwrite-strings"
    ;; "-Wno-missing-field-initializers" "-Wnon-virtual-dtor"
    ;; "-Weffc++" "-Wold-style-cast" "-Woverloaded-virtual"
    ))

(defun flymake-extract-includes-from-makefile ()
  (let ((buf (current-buffer))
        (dir (file-name-directory (or (buffer-file-name) ""))))
    (with-temp-buffer
      (if (file-readable-p (concat dir "Makefile"))
          (progn
            (insert-file-contents (concat dir "Makefile") nil nil nil t)
            (goto-char 0)
            (if (re-search-forward "^INCLUDE\\s-*=\\s-*\\(.*\\)$" nil t nil)
                (let ((includes (split-string (match-string 1) " \t\r\n")))
                  (with-current-buffer buf
                    (set (make-local-variable 'flymake-cc-command-args)
                         (append includes flymake-cc-command-args))))))))))

(autoload 'flymake-init-create-temp-buffer-copy "flymake")
(defun flymake-cc-init ()
  (let ((cmd (cond
              ((eq major-mode 'c-mode) flymake-c-command)
              ((eq major-mode 'c++-mode) flymake-cc-command)
              (t nil))))
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list cmd (append flymake-cc-command-args (list local-file))))))

(with-eval-after-load-feature 'flymake
  (push '("\\.c$" flymake-cc-init) flymake-allowed-file-name-masks)
  (push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks))

;; C
(autoload 'c-mode "cc-mode")
(setq auto-mode-alist
      (append '(("\\.h$" . c-mode)
                ("\\.c$" . c-mode))
              auto-mode-alist))
(add-hook 'c-mode-hook
          #'(lambda ()
              (c-set-style "stroustrup")
              (c-set-offset 'innamespace 0)
              (when (boundp 'c-basic-offset) (setq c-basic-offset 2))
              (flymake-extract-includes-from-makefile)
              (flymake-mode t)))

;; C++
(autoload 'c++-mode "cc-mode")
(setq auto-mode-alist
      (append '(;("\\.h$" . c++-mode)
                ("\\.hpp$" . c++-mode)
                ("\\.cpp$" . c++-mode)
                ("\\.hxx$" . c++-mode)
                ("\\.cxx$" . c++-mode))
              auto-mode-alist))
(add-hook 'c++-mode-hook
          #'(lambda ()
              (c-set-style "stroustrup")
              (c-set-offset 'innamespace 0)
              (when (boundp 'c-basic-offset) (setq c-basic-offset 2))
              (flymake-extract-includes-from-makefile)
              (flymake-mode t)))
