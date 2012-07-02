;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perl

(autoload 'cperl-mode "cperl-mode"
  "alternate mode for editing Perl programs" t)
(setq auto-mode-alist
      (append '(("\\.pl$" . cperl-mode)
                ("\\.pm$" . cperl-mode)
                ("\\.t$" . cperl-mode))
              auto-mode-alist))
(setq cperl-indent-level 4
      cperl-continued-statement-offset 4
      cperl-close-paren-offset -4
      cperl-comment-column 40
      cperl-highlight-variables-indiscriminately t
      cperl-indent-parens-as-block t
      cperl-label-offset -4
      cperl-tab-always-indent nil
      cperl-font-lock t)

;; auto-insert package name
(setq auto-insert-alist
      (append '(("\\.pm$" . ["insert.pm" my-template]))
              auto-insert-alist))
(setq template-replacement-alist
      (append
       '(("%Perl-package%" . (lambda () (pm2package (buffer-file-name)))))
       template-replacement-alist))

(defun pm2package (fname)
  (let ((lib (findlib (split-string fname "/"))))
    (let ((path (join-to-string "::" (if (consp lib) (cdr lib) lib))))
      (replace-regexp-in-string "\\.pm$" "" path))))

(defun findlib (dirs)
  (let ((subdirs (member "lib" dirs)))
    (unless (null subdirs) (or (findlib (cdr subdirs)) subdirs))))

;; set-perl5lib
;; add lib to @INC
;; http://svn.coderepos.org/share/lang/elisp/set-perl5lib/set-perl5lib.el
(require 'set-perl5lib)

;; http://unknownplace.org/memo/2007/12/21#e001
(defvar flymake-perl-err-line-patterns
  '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))

(defconst flymake-allowed-perl-file-name-masks
  '(("\\.pl$" flymake-perl-init)
    ("\\.pm$" flymake-perl-init)
    ("\\.t$" flymake-perl-init)
    ("\\.cgi$" flymake-perl-init)))

(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc" local-file))))

(defun flymake-perl-load ()
  (interactive)
  (defadvice flymake-post-syntax-check
    (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-perl-file-name-masks))
  (setq flymake-err-line-patterns flymake-perl-err-line-patterns)
  (set-perl5lib)
  (flymake-mode t))

(add-hook 'cperl-mode-hook 'flymake-perl-load)
