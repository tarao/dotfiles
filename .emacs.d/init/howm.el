(setq howm-directory "~/docs/howm/")
(setq howm-file-name-format "%Y/%m/%Y-%m-%dT%H%M%S.howm")
(setq howm-keyword-file "~/docs/.howm-keys")
(setq howm-history-file "~/docs/.howm-history")
(setq howm-menu-lang 'en)
(setq auto-mode-alist (append '(("\\.howm$" . rdoc-mode)) auto-mode-alist))

(when (require 'howm nil t)
  ;; evil
  (eval-after-load 'evil
    '(progn
       ;; menu
       (evil-make-overriding-map howm-menu-mode-map 'normal)
       (add-hook 'howm-menu-hook
                 '(lambda () (define-key howm-menu-mode-local-map ":" nil)))

       ;; list
       (evil-make-overriding-map howm-view-summary-mode-map 'normal)
       (evil-define-key 'normal howm-view-summary-mode-map
         "j" (lookup-key evil-motion-state-map "j")
         "k" (lookup-key evil-motion-state-map "k")
         "J" 'evil-scroll-down
         "K" 'evil-scroll-up))))
