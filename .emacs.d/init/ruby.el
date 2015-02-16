(bundle rdoc-mode)
(el-get-lock-unlock 'inf-ruby)
(bundle inf-ruby
  (add-hook 'ruby-mode-hook #'inf-ruby-keys))

(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)
                ("Rakefile$" . ruby-mode)
                ("\\.rdoc$" . rdoc-mode)) auto-mode-alist))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(setq-default ruby-deep-indent-paren-style nil)
