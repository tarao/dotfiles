;;; C++
(autoload 'c++-mode "cc-mode")
(add-hook 'c++-mode-hook
          '(lambda ()
             (c-set-style "stroustrup")
             (c-set-offset 'innamespace 0)
             (setq c-basic-offset 2)))
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)
                ("\\.hpp$" . c++-mode)
                ("\\.cpp$" . c++-mode)
                ("\\.hxx$" . c++-mode)
                ("\\.cxx$" . c++-mode)) auto-mode-alist))

(setq flymake-cc-command-opt
      '("-fsyntax-only"
;;         "-std=c++98" "-pedantic-errors"
        "-Wall" "-Wextra"
;;         "-Wcast-qual" "-Wwrite-strings"
;;         "-Wno-missing-field-initializers" "-Wnon-virtual-dtor"
;;         "-Weffc++" "-Wold-style-cast" "-Woverloaded-virtual"
        ))

(defun flymake-cc-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "g++" (append flymake-cc-command-opt (list local-file)))))

(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

(add-hook 'c++-mode-hook
          '(lambda ()
             (progn
               (hs-enable)
               (flymake-extract-includes-from-makefile)
               (flymake-mode t))))

(defun flymake-extract-includes-from-makefile ()
  (let ((tmp-buf-name "*flymake-temp-buf-for-makefile*")
        (buf (current-buffer))
        (dir (file-name-directory (buffer-file-name))))
    (if (file-readable-p (concat dir "Makefile"))
        (progn
          (set-buffer (get-buffer-create tmp-buf-name))
          (insert-file-contents (concat dir "Makefile") nil nil nil t)
          (goto-char 0)
          (if (re-search-forward "^INCLUDE\\s-*=\\s-*\\(.*\\)$" nil t nil)
              (let ((includes (split-string (match-string 1) " \t\r\n")))
                (kill-buffer tmp-buf-name)
                (set-buffer buf)
                (make-variable-buffer-local 'flymake-cc-command-opt)
                (setq flymake-cc-command-opt
                      (append includes flymake-cc-command-opt))))))))
