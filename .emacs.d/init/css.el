(autoload 'css-mode "css-mode" "Major mode for editing CSS" t)
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))
(setq cssm-newline-before-closing-bracket t)
