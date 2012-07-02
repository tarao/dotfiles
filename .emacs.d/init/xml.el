;; nxml-mode
(load "rng-auto")
(add-to-list 'auto-mode-alist
             (cons (concat "\\."
                           (regexp-opt
                            '("xml" "xsd" "sch" "rng" "xslt"
                              "svg" "rss" "mxml") t)
                           "\\'")
                   'nxml-mode))
(unify-8859-on-decoding-mode)
(setq magic-mode-alist
      (cons '("<\\?xml " . nxml-mode)
            magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)

;; use nxml-mode instead of sgml, xml or html mode.
(mapc
 (lambda (pair)
   (if (or (eq (cdr pair) 'xml-mode)
           (eq (cdr pair) 'sgml-mode)
           (eq (cdr pair) 'html-mode))
       (setcdr pair 'nxml-mode)))
 magic-mode-alist)

;; use zen-coding
(require 'zencoding-mode)
(require 'zencoding-trie)
(add-hook 'nxml-mode-hook 'zencoding-mode)
(define-key zencoding-mode-keymap (kbd "C-j") 'zencoding-expand-line)
(define-key zencoding-preview-keymap (kbd "RET") 'zencoding-preview-accept)

;; xquery mode
(autoload 'xquery-mode "xquery-mode" "Major mode for editing xquery" t)
(setq auto-mode-alist (cons '("\\.xquery$" . xquery-mode) auto-mode-alist))

(autoload 'yaml-mode "yaml-mode"
  "Mode for editing YAML files" t)
(setq auto-mode-alist
      (append '(("\\.yml$" . yaml-mode)
                ("\\.yaml$" . yaml-mode)) auto-mode-alist))
