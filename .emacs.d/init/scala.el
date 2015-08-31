(setq-default
 ensime-completion-style 'auto-complete
 ensime-ac-enable-argument-placeholders nil
 ensime-ac-override-settings nil)

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
        (when (null ensime-buffer)
          (noflet ((ensime-config-find (&rest _) file))
            (save-window-excursion
              (ensime)))))))

  (defun scala/ensime-project-name-from-config (file)
    (eval-and-compile (require 'ensime))
    (let ((config (ensime-config-load file)))
      (plist-get config :name)))

  (defun scala/ensime-buffer-for-file (file)
    "Find the Ensime server buffer corresponding to FILE."
    (eval-and-compile (require 'ensime))
    (eval-and-compile (require 'dash))
    (eval-and-compile (require 's))
    (let* ((config-file (ensime-config-find-file file))
           (name (and config-file
                      (scala/ensime-project-name-from-config config-file)))
           (default-directory (file-name-directory file)))
      (when name
        (--first (-when-let (bufname (buffer-name it))
                   (and (s-contains? "inferior-ensime-server" bufname)
                        (s-contains? name bufname)))
                 (buffer-list)))))

  (defun scala/enable-eldoc ()
    "Show error message or type name at point by Eldoc."
    (eval-and-compile (require 'ensime))
    (setq-local eldoc-documentation-function
                #'(lambda ()
                    (when (ensime-connected-p)
                      (let ((err (mapconcat 'identity
                                            (ensime-errors-at (point))
                                            "\n")))
                        (or (and err (not (string= err "")) (message err))
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

  (defun scala/ac-trigger-key-command (orig-fun &rest args)
    (if ensime-mode
        (let ((ac-sources '(ac-source-ensime-completions))
              (ac-use-comphist nil)
              (ac-auto-show-menu 0.5)
              (ac-candidates-cache nil)
              (ac-auto-start nil)
              (ac-expand-on-auto-complete t)
              (ac-use-fuzzy nil)
              (ac-dwim nil)
              (ac-use-quick-help t)
              (ac-delete-dups nil)
              (ac-ignore-case t))
          (apply orig-fun args))
      (apply orig-fun args)))
  (advice-add 'ac-trigger-key-command :around #'scala/ac-trigger-key-command)

  (defun scala/completing-dot-ac ()
    (eval-and-compile (require 'auto-complete))
    (insert ".")
    (ac-trigger-key-command t))

  (defmacro scala/with-project-sbt (&rest form)
    `(progn
       (eval-and-compile (require 'projectile))
       (eval-and-compile (require 'sbt-mode))
       (let* ((file (or (buffer-file-name) (error "Visiting no file")))
              (dir (file-name-directory file))
              (dir (let ((default-directory dir)) (projectile-project-p))))
         (when dir
           (setq sbt:buffer-project-root dir)
           (condition-case err
               (progn ,@form)
             (error (error err)))))))

  (defun scala/call-sbt-command (command &rest args)
    (scala/with-project-sbt
     (let (buf (get-buffer-create (sbt:buffer-name)))
       (apply 'call-process sbt:program-name nil buf t args))))

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

  (defun ensime-cleanup ()
    "Shutdown and destroy connection buffer."
    (interactive)
    (ensime-shutdown)
    (let* ((buf (buffer-file-name))
           (ensime-buffer (scala/ensime-buffer-for-file buf)))
      (when ensime-buffer (kill-buffer ensime-buffer))))

  (defun ensime-restart ()
    "Restart the ensime server."
    (interactive)
    (ensime-cleanup)
    (scala/maybe-start-ensime))

  (defun ensime-gen-and-restart ()
    "Regenerate `.ensime' file and restart the ensime server."
    (interactive)
    (progn
      (message "Regenerating .ensime ...")
      (when (= 0 (scala/call-sbt-command "gen-ensime"))
        (ensime-restart))))

  ;; Configuration

  (defadvice ensime-search-mode (after ignore-trailing-whitespace activate)
    (with-current-buffer ensime-search-target-buffer-name
      (setq show-trailing-whitespace nil)))

  ;; Initialization

  (defun tarao/enable-eldoc ()
    (set (make-local-variable 'eldoc-idle-delay) 0.5)
    (scala/enable-eldoc))

  (defun tarao/configure-scala ()
    (eval-and-compile (require 'ensime))
    (eval-and-compile (require 'auto-complete))
    (scala/configure-ensime)
    (scala/maybe-start-ensime)
    (make-local-variable 'ac-trigger-key)
    (ac-set-trigger-key "TAB")
    (unless (ensime-config-find-file (buffer-file-name))
      (flycheck-mode +1)))

  (defadvice ensime (after ensime-disable-flycheck activate)
    (flycheck-mode -1))

  (add-hook 'ensime-mode-hook #'tarao/enable-eldoc)
  (add-hook 'scala-mode-hook #'tarao/configure-scala)
  )
