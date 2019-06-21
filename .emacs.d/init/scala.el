(setq-default
 sbt:ansi-support t
 lsp-prefer-flymake nil
 )
(defconst metals-scala-version "2.12")
(defconst metals-artifact-template "org.scalameta%smetals_%s")
(defconst bloop-releases-api
  "https://api.github.com/repos/scalacenter/bloop/releases/latest")

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
(bundle lsp-mode)
(bundle lsp-ui
  (with-eval-after-load-feature 'lsp-ui
    (set-face-background 'lsp-ui-sideline-global "#444444")
    ))

;; metals

(defun scala/install-coursier ()
  (unless (executable-find "coursier")
    (let ((output (expand-file-name "coursier" user-emacs-bin-directory)))
      (call-process-shell-command
       (format "curl -sL -o %s https://git.io/coursier"
               (shell-quote-argument output)))
      (call-process-shell-command
       (format "chmod a+x %s" (shell-quote-argument output))))))

(defun scala/metals-latest-version ()
  (let* ((q (format metals-artifact-template "+" metals-scala-version))
         (search-url "https://search.maven.org/solrsearch/select")
         (cmds (list
                (format "curl -s '%s?q=%s&rows=20&wt=json'" search-url q)
                "jq -r '.response.docs[]|.g+\":\"+.a+\":\"+.latestVersion'"))
         (cmd (mapconcat #'identity cmds " | "))
         (results (split-string (shell-command-to-string cmd) "\n" t))
         (pattern (format metals-artifact-template ":" "[0-9.\\]+")))
    (car-safe
     (reverse
      (seq-sort
       #'string<
       (seq-filter (lambda (s) (string-match-p pattern s)) results))))))

(defun scala/metals-binary ()
  (expand-file-name "metals-emacs" user-emacs-bin-directory))

(defun scala/install-metals ()
  (unless (executable-find "metals-emacs")
    (let ((output (scala/metals-binary))
          (artifact (scala/metals-latest-version))
          (java-opts (list "--java-opt" "-Xss4m"
                           "--java-opt" "-Xms100m"
                           "--java-opt" "-Dmetals.client=emacs"))
          (repos (list "-r" "bintray:scalacenter/releases"
                       "-r" "sonatype:snapshots"))
          (buf "*scala/install-metals*"))
      (when artifact
        (message "Install %s to %s" artifact output)
        (save-window-excursion
          (switch-to-buffer buf)
          (redisplay t)
          (call-process-shell-command
           (format "coursier bootstrap %s %s %s -o %s -f"
                   (mapconcat #'shell-quote-argument java-opts " ")
                   artifact
                   (mapconcat #'shell-quote-argument repos " ")
                   output) nil buf t))))))

(defun scala/reinstall-metals ()
  (interactive)
  (call-process-shell-command
   (format "rm -f %s" (shell-quote-argument (scala/metals-binary))))
  (scala/install-metals))

;; bloop

(defun scala/bloop-installer-latest-version ()
  (let* ((cmds (list
                (format "curl -sL %s" bloop-releases-api)
                "jq -r '.assets[].browser_download_url'"))
         (cmd (mapconcat #'identity cmds " | "))
         (results (split-string (shell-command-to-string cmd) "\n" t)))
    (car-safe
     (seq-filter (lambda (s) (string-suffix-p "/install.py" s)) results))))

(defun scala/bloop-binary ()
  (expand-file-name "bloop" user-emacs-bin-directory))

(defun scala/install-bloop ()
  (unless (executable-find "bloop")
    (let* ((installer-url (scala/bloop-installer-latest-version))
           (output (scala/bloop-binary))
           (cmds (list
                  (format "curl -sL %s"
                          (shell-quote-argument installer-url))
                  (format "python - --dest %s"
                          (shell-quote-argument user-emacs-bin-directory))))
           (cmd (mapconcat #'identity cmds " | "))
           (buf "*scala/install-bloop*"))
      (message "Install bloop to %s" output)
      (save-window-excursion
        (switch-to-buffer buf)
        (redisplay t)
        (call-process-shell-command cmd nil buf t)))))

(defun scala/reinstall-bloop ()
  (interactive)
  (call-process-shell-command
   (format "rm -f %s" (shell-quote-argument (scala/bloop-binary))))
  (scala/install-bloop))

(defun scala/init-bloop ()
  (scala/install-bloop)
  (unless (= 0 (call-process-shell-command "bloop about"))
    (message "Start bloop server")
    (start-process "bloop-server" "*bloop-server*" "bloop" "server")))

;; scala-mode

(add-hook 'scala-mode-hook
          '(lambda ()
             (scala/install-coursier)
             (scala/install-metals)
             (scala/init-bloop)
             (flycheck-mode)
             (auto-complete-mode -1)
             (require 'yasnippet)
             (require 'lsp-ui)
             (lsp)))
