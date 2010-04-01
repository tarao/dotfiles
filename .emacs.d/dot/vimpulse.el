;; viper minor mode
(setq viper-custom-file-name (locate-library "dot/.viper"))
(setq viper-mode t)
(setq viper-toggle-key (kbd "C-x C-z"))
(setq viper-ex-style-motion nil)
(setq viper-ex-style-editing nil)
(setq blink-matching-paren t)
(require 'viper)
(setq viper-vi-state-id "")
(setq viper-insert-state-id "INSERT")
(setq viper-visual-characterwise-state-id "VISUAL")
(setq viper-visual-linewise-state-id "VLINE")
(setq viper-visual-blockwise-state-id "VBLOCK")
(setq viper-emacs-state-id "x")

;; viper-mode patches
(require 'hexl-viper-patch)
(defadvice viper-maybe-checkout (around viper-dont-ask-checkout activate) nil)

(setq vimpulse-want-vi-keys-in-dired nil)
(setq woman-use-own-frame nil) ; don't create new frame for manpages
(require 'vimpulse)

;; viper-mode keymaps
(define-key vimpulse-visual-basic-map
  (kbd ";") (lambda () (interactive) (viper-ex t)))
(define-key viper-vi-basic-map
  (kbd ";") 'viper-ex)
(define-key viper-vi-global-user-map
  (kbd ":") 'anything)
(define-key viper-vi-basic-map
  (kbd "TAB") nil)
(define-key viper-vi-global-user-map
  (kbd "C-w") 'kill-region)
(define-key viper-insert-global-user-map
  (kbd "C-w") 'kill-region)
(define-key viper-insert-basic-map
  (kbd "C-p") nil)
(define-key viper-insert-basic-map
  (kbd "C-n") nil)

;; vimpulse patches

;; '* i w' at the beginning of word
(defun vimpulse-text-object-bounds
  (backward-func forward-func &optional arg pos)
  "Returns the boundaries of one or more text objects.
BACKWARD-FUNC moves point to the object's beginning,
FORWARD-FUNC moves to its end. Schematically,

\(vimpulse-text-object-bounds <beg-of-object> <end-of-object>)

Boundaries are returned as (START END). If specified,
ARG controls the number of objects and POS the starting point
\(`point' by default)."
  (let (beg end)
    (setq arg (or arg 1))
    ;; If ARG is negative, swap BACKWARD-FUNC and FORWARD-FUNC
    (cond
     ((> 0 arg)
      (setq beg backward-func)
      (setq backward-func forward-func)
      (setq forward-func beg))
     ((= 0 arg)
      (setq arg 1)))
    ;; To avoid errors when hitting upon buffer boundaries,
    ;; we make extensive use of `condition-case' ...
    (save-excursion
      (when pos
        (goto-char pos))
      ;; We might already be at the ending character --
      ;; go one character back so we don't run past it.
      (condition-case nil
          (when (> 0 arg) (forward-char))
        (error nil))
      (condition-case nil
          (funcall forward-func 1)
        (error nil))
      (condition-case nil
          (funcall backward-func 1)
        (error nil))
      (setq beg (point))
      (condition-case nil
          (funcall forward-func (abs arg))
        (error nil))
      (setq end (point)))
    (sort (list beg end) '<)))
