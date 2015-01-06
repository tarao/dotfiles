;; nxml-mode
(add-to-list 'auto-mode-alist
             (cons (concat "\\."
                           (regexp-opt
                            '("htm" "html" "xml" "xsd" "sch" "rng" "xslt"
                              "svg" "rss" "mxml") t)
                           "\\'")
                   'nxml-mode))
(setq magic-mode-alist
      (cons '("<\\?xml " . nxml-mode)
            magic-mode-alist))

;; use nxml-mode instead of sgml, xml or html mode.
(mapc
 #'(lambda (pair)
     (if (or (eq (cdr pair) 'xml-mode)
             (eq (cdr pair) 'sgml-mode)
             (eq (cdr pair) 'html-mode))
         (setcdr pair 'nxml-mode)))
 magic-mode-alist)

;; template for XHTML
(add-to-list 'auto-insert-alist
             '("\\.xhtml$" . ["insert.xhtml" template-replacer]))

;; zencoding
(bundle zencoding-mode
  (setq-default zencoding-preview-default nil ; no preview
                zencoding-insert-flash-time 0.2)
  (add-hook 'nxml-mode-hook #'zencoding-mode)

  (with-eval-after-load-feature 'zencoding-mode
    (define-key zencoding-mode-keymap (kbd "C-j")
      #'zencoding-expand-line)
    (define-key zencoding-preview-keymap (kbd "RET")
      #'zencoding-preview-accept)))

;; YAML
(bundle yaml-mode)
(setq auto-mode-alist
      (append '(("\\.yml$" . yaml-mode)
                ("\\.yaml$" . yaml-mode)) auto-mode-alist))
