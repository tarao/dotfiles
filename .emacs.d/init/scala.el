(setq-default
 sbt:ansi-support t
 lsp-prefer-flymake nil
 )
(defconst metals-scala-version "2.12")
(defconst metals-version "0.6.1")

(bundle flycheck)
(bundle scala-mode)
(bundle sbt-mode
  (defun scala/sbt-do-test ()
    (interactive)
    (sbt-command "test"))

  (defun scala/sbt-do-compile ()
    (interactive)
    (sbt-command "test:compile"))

  (defun scala/sbt-do-clean ()
    (interactive)
    (sbt-command "clean"))

  (defun scala/sbt-do-console ()
    (interactive)
    (sbt-command "consoleQuick")))

(bundle yasnippet)
(bundle company-mode)
(bundle company-lsp)
(bundle helm-lsp)
(bundle lsp-ui
  (with-eval-after-load-feature 'lsp-ui
    (set-face-background 'lsp-ui-sideline-global "#444444")
    ))
(bundle lsp-mode
  (defun scala/init-coursier ()
    (unless (executable-find "coursier")
      (let ((output (expand-file-name "coursier" user-emacs-bin-directory)))
        (shell-command (format "curl -L -o %s https://git.io/coursier"
                               (shell-quote-argument output)))
        (shell-command (format "chmod a+x %s"
                               (shell-quote-argument output))))))

  (defun scala/init-metals ()
    (unless (executable-find "metals-emacs")
      (let ((output (expand-file-name "metals-emacs" user-emacs-bin-directory))
            (archive (format "org.scalameta:metals_%s:%s"
                             metals-scala-version
                             metals-version))
            (java-opts (list "--java-opt" "-Xss4m"
                             "--java-opt" "-Xms100m"
                             "--java-opt" "-Dmetals.client=emacs"))
            (repos (list "-r" "bintray:scalacenter/releases"
                         "-r" "sonatype:snapshots"))
            (buf "*scala/init-metals*"))
        (save-window-excursion
          (switch-to-buffer buf)
          (call-process-shell-command
           (format "coursier bootstrap %s %s %s -o %s -f"
                   (mapconcat #'shell-quote-argument java-opts " ")
                   archive
                   (mapconcat #'shell-quote-argument repos " ")
                   output) nil buf t)))))
  (add-hook 'scala-mode-hook
            #'(lambda ()
                (scala/init-coursier)
                (scala/init-metals)
                (flycheck-mode)
                (auto-complete-mode -1)
                (require 'yasnippet)
                (require 'lsp-ui)
                (lsp))))
