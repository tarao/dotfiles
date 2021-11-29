;; hatena:markup-mode
(defconst hatena-markup-mode-files
  '("/tarao\\.hatenablog\\.com/.*\\.md$"
    "/localweb\\.hatenablog\\.com/.*\\.md$"))
(setq auto-mode-alist
      (append (mapcar #'(lambda (x)
                          (cons x 'hatena:markup-mode))
                      hatena-markup-mode-files)
              auto-mode-alist))
