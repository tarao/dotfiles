(setq-default ensime-completion-style 'auto-complete)

(bundle flycheck)
(bundle scala-mode2)
(bundle sbt-mode)
(bundle projectile)
(bundle noflet)
(bundle yasnippet)
(bundle company-mode)
(bundle ensime
  ;; See https://github.com/syl20bnr/spacemacs/tree/master/contrib/lang/scala

  (defun scala/configure-ensime ()
    "Ensure the file exists before starting `ensime-mode'."
    (eval-and-compile (require 'ensime))
    (if (file-exists-p (buffer-file-name))
        (ensime-mode +1)
      (add-hook 'after-save-hook #'(lambda () (ensime-mode +1)) nil t)))

  (defun scala/maybe-start-ensime ()
    (eval-and-compile (require 'ensime))
    (eval-and-compile (require 'noflet))
    (when (buffer-file-name)
      (let ((ensime-buffer (scala/ensime-buffer-for-file (buffer-file-name)))
            (file (ensime-config-find-file (buffer-file-name))))
        ;; ignore if there is no .ensime for the project
        (when (and file (null ensime-buffer))
          (noflet ((ensime-config-find (&rest _) file))
            (save-window-excursion
              (ensime)))))))

  (defun scala/ensime-buffer-for-file (file)
    "Find the Ensime server buffer corresponding to FILE."
    (eval-and-compile (require 'dash))
    (eval-and-compile (require 's))
    (eval-and-compile (require 'projectile))
    (let ((default-directory (file-name-directory file)))
      (-when-let (project-name (projectile-project-p))
        (--first (-when-let (bufname (buffer-name it))
                   (and (s-contains? "inferior-ensime-server" bufname)
                        (s-contains? (file-name-nondirectory
                                      (directory-file-name project-name))
                                     bufname)))
                 (buffer-list)))))

  (defun scala/enable-eldoc ()
    "Show error message or type name at point by Eldoc."
    (eval-and-compile (require 'ensime))
    (setq-local eldoc-documentation-function
                #'(lambda ()
                    (when (ensime-connected-p)
                      (let ((err (ensime-print-errors-at-point)))
                        (or (and err (not (string= err "")) err)
                            (ensime-print-type-at-point))))))
    (eldoc-mode +1))

  (defun scala/completing-dot-company ()
    (eval-and-compile (require 'company))
    (cond (company-backend
           (company-complete-selection)
           (scala/completing-dot))
          (t
           (insert ".")
           (company-complete))))

  (defun scala/completing-dot-ac ()
    (eval-and-compile (require 'auto-complete))
    (insert ".")
    (ac-trigger-key-command t))

  ;; Interactive commands

  (defun scala/completing-dot ()
    "Insert a period and show company completions."
    (interactive "*")
    (eval-and-compile (require 'ensime))
    (eval-and-compile (require 's))
    (when (s-matches? (rx (+ (not space)))
                      (buffer-substring (line-beginning-position) (point)))
      (delete-horizontal-space t))
    (cond ((not (and (ensime-connected-p) ensime-completion-style))
           (insert "."))
          ((eq ensime-completion-style 'company)
           (scala/completing-dot-company))
          ((eq ensime-completion-style 'auto-complete)
           (scala/completing-dot-ac))))

  (defmacro scala/with-project-sbt (&rest form)
    (eval-and-compile (require 'projectile))
    (eval-and-compile (require 'sbt-mode))
    `(let* ((file (or (buffer-file-name) (error "Visiting no file")))
            (dir (file-name-directory file))
            (dir (let ((default-directory dir)) (projectile-project-p))))
       (when dir
         (setq sbt:buffer-project-root dir)
         (condition-case err
             (progn ,@form)
           (error (error err))))))

  (defun scala/repl ()
    "Start REPL for the project"
    (interactive)
    (eval-and-compile (require 'sbt-mode))
    (scala/with-project-sbt
     (prog1 (sbt-start)
       (sbt-command "console"))))

  (defun sbt:send-buffer ()
    "Send buffer content to shell."
    (interactive)
    (eval-and-compile (require 'sbt-mode))
    (sbt:send-region (point-min) (point-max)))

  (defun ensime-gen-and-restart()
    "Regenerate `.ensime' file and restart the ensime server."
    (interactive)
    (progn
      (sbt-command "gen-ensime")
      (ensime-shutdown)
      (ensime)))

  ;; Initialization

  (defun tarao/configure-scala ()
    (eval-and-compile (require 'ensime))
    (scala/configure-ensime)
    (scala/maybe-start-ensime)
    (unless (ensime-config-find-file (buffer-file-name))
      (flycheck-mode +1)))

  (defadvice ensime (after ensime-disable-flycheck activate)
    (flycheck-mode -1))

  (add-hook 'ensime-mode-hook #'scala/enable-eldoc)
  (add-hook 'scala-mode-hook #'tarao/configure-scala)
  )
