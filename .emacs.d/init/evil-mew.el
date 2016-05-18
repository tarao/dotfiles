(bundle evil
  (defun mew-draft-evil-open-below (count)
    (interactive "p")
    (if (get-text-property (point) 'read-only)
        (progn
          (forward-line count)
          (evil-open-above 1))
      (evil-open-below count)))
  (with-eval-after-load-feature 'mew-key
    ;; mew-summary-mode key maps
    (evil-make-overriding-map mew-summary-mode-map 'normal)
    (evil-add-hjkl-bindings mew-summary-mode-map 'normal
      "h" (lookup-key mew-summary-mode-map "h")
      "l" (lookup-key mew-summary-mode-map "l")
      "G" (lookup-key evil-motion-state-map "G")
      "J" (lookup-key evil-motion-state-map "J")
      "K" (lookup-key evil-motion-state-map "K")
      ";" (lookup-key evil-motion-state-map ";"))
    ;; mew-message-mode key maps
    (evil-make-overriding-map mew-message-mode-map 'normal)
    (evil-add-hjkl-bindings mew-message-mode-map 'normal)
    ;; mew-draft-mode key maps
    (dolist (map (list mew-draft-header-map mew-draft-body-map))
      (evil-define-key 'normal map
        "o" #'mew-draft-evil-open-below
        "q" #'mew-draft-kill))))
