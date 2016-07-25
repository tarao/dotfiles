;; modules
(require 'skk-hint)

;; customization
(setq-default skk-egg-like-newline t
              skk-henkan-strict-okuri-precedence t
              skk-show-annotation t
              skk-dcomp-activate 'eolp
              ;; skk-dcomp-multiple-activate t
              skk-kutouten-type 'en)

;; face
(when (memq 'skk-dcomp-multiple-face (face-list))
  (set-face-foreground 'skk-dcomp-multiple-face "black")
  (set-face-background 'skk-dcomp-multiple-face "lightgray")
  (set-face-foreground 'skk-dcomp-multiple-trailing-face "yellow")
  (set-face-background 'skk-dcomp-multiple-selected-face "blue"))

;; dictionary

(unless (and (boundp 'skk-server-host) skk-server-host)
  (setq skk-server-host "localhost"
        skk-server-portnum 1179))

(defvar skk-fallback-large-jisyo "/usr/share/skk/SKK-JISYO.L")
(defvar skk-fallback-extra-jisyo-files
  '("/usr/share/skk/SKK-JISYO.pubdic+"
    "/usr/share/skk/SKK-JISYO.jinmei"
    "/usr/share/skk/SKK-JISYO.geo"
    "/usr/share/skk/SKK-JISYO.JIS2"
    "/usr/share/skk/SKK-JISYO.JIS3_4"
    "/usr/share/skk/SKK-JISYO.JIS2004"
    "/usr/share/skk/SKK-JISYO.zipcode"))

(require 'skk-server)
(unless (skk-server-live-p (skk-open-server))
  ;; deactivate skk-server-host
  (setq skk-server-host nil)

  ;; fallback to local files
  (unless (listp skk-fallback-large-jisyo)
    (setq skk-fallback-large-jisyo (list skk-fallback-large-jisyo)))
  (let ((list skk-fallback-large-jisyo))
    (while (and (not skk-large-jisyo) list)
      (when (file-readable-p (car list)) (setq skk-large-jisyo (car list)))
      (setq list (cdr list))))
  (dolist (file skk-fallback-extra-jisyo-files)
    (when (file-readable-p file)
      (add-to-list 'skk-extra-jisyo-file-list file t))))

;; kakutei hack
(defun skk-j-mode-before-kakutei (arg)
  (interactive "P")
  (let ((valid-buffer '(lisp-interaction-mode)))
    (cond
     ((and (null (skk-in-minibuffer-p))
           (null skk-henkan-mode)
           (member major-mode valid-buffer))
      (skk-emulate-original-map arg))
     (t
      (skk-kakutei arg)))))
(add-to-list 'skk-rom-kana-rule-list
             '(skk-kakutei-key nil skk-j-mode-before-kakutei))

;; rom -> kana rules

(defvar my-skk-rom-kana-rule-list-base
      (append
       '(
         ("h." nil ".")
         ("h," nil ",")
         ("h!" nil "!")
         ("h?" nil "?")
         ("h;" nil ";")
         ("h:" nil ":")
         ("h-" nil "-")
         ("h~" nil "~")
         ("h/" nil "/")
         ("h@" nil "@")
         ("h=" nil "=")
         ("h(" nil "(")
         ("h)" nil ")")
         ("h[" nil "[")
         ("h]" nil "]")
         ("h<" nil "<")
         ("h>" nil ">")
         ("h\\" nil "\\")
         ("h$" nil "$")
         ("ht" nil "\t")
         ("z " nil "　")
         ("z." nil "。")
         ("z," nil "、")
         ("z!" nil "！")
         ("z?" nil "？")
         ("z;" nil "；")
         ("z:" nil "：")
         ("z-" nil "ー")
         ("z~" nil "〜")
         ("z/" nil "・")
         ("z@" nil "＠")
         ("z=" nil "＝")
         ("z(" nil "（")
         ("z)" nil "）")
         ("z[" nil "「")
         ("z]" nil "」")
         ("z<" nil "＜")
         ("z>" nil "＞")
         ("zz-" nil "…")
         ("zz[" nil "『")
         ("zz]" nil "』")
         ("ca" nil "∧")
         ("co" nil "∨")
         ("cn" nil "¬")
         ("ci" nil "⊃")
         ("cf" nil "⊥")
         ("cY" nil "⇒")
         ("cd" nil "≡")
         ("cA" nil "∀")
         ("cE" nil "∃")
         ("ce" nil "∈")
         ("cu" nil "Π")
         ("cl" nil "λ")
         ("c<" nil "《")
         ("c>" nil "》")
         ("cp" nil "├")
         )
       skk-rom-kana-rule-list))
(defvar my-skk-rom-kana-rule-list-en
      (append
       '(
         ("." nil ".")
         ("," nil ",")
         ("!" nil "!")
         ("?" nil "?")
         (";" nil ";")
         (":" nil ":")
       ; ("-" nil "-")
         ("~" nil "~")
       ; ("/" nil "/")
         ("@" nil "@")
         ("=" nil "=")
         ("(" nil "(")
         (")" nil ")")
         ("[" nil "[")
         ("]" nil "]")
         ("<" nil "<")
         (">" nil ">")
       ; ("\\" nil "\\")
       ; ("$" nil "$")
         )
       my-skk-rom-kana-rule-list-base))
(defvar my-skk-rom-kana-rule-list-ja
      (append
       '(
         ("." nil "。")
         ("," nil "、")
         ("!" nil "！")
         ("?" nil "？")
         ("-" nil "ー")
         ("@" nil "＠")
         ("[" nil "「")
         ("]" nil "」")
         )
       my-skk-rom-kana-rule-list-base))
(defvar my-skk-rom-kana-rule-list-JA
      (append
       '(
         ("." nil "．")
         ("," nil "，")
         ("!" nil "！")
         ("?" nil "？")
         ("-" nil "ー")
         ("@" nil "＠")
         ("[" nil "「")
         ("]" nil "」")
         )
       my-skk-rom-kana-rule-list-base))
(defvar my-skk-rom-kana-rule-list-Ja
      (append
       '(
         ("." nil "。")
         ("," nil "，")
         ("!" nil "！")
         ("?" nil "？")
         ("-" nil "ー")
         ("@" nil "＠")
         ("[" nil "「")
         ("]" nil "」")
         )
       my-skk-rom-kana-rule-list-base))
(defun skk-reconstruct-rule-tree ()
  (setq skk-rule-tree (skk-compile-rule-list
                       skk-rom-kana-base-rule-list
                       skk-rom-kana-rule-list)))
(defun skk-use-en-signs ()
  (interactive)
  (setq skk-rom-kana-rule-list my-skk-rom-kana-rule-list-en)
  (skk-reconstruct-rule-tree))
(defun skk-use-ja-signs ()
  (interactive)
  (setq skk-rom-kana-rule-list my-skk-rom-kana-rule-list-ja)
  (skk-reconstruct-rule-tree))
(defun skk-use-JA-signs ()
  (interactive)
  (setq skk-rom-kana-rule-list my-skk-rom-kana-rule-list-JA)
  (skk-reconstruct-rule-tree))
(defun skk-use-Ja-signs ()
  (interactive)
  (setq skk-rom-kana-rule-list my-skk-rom-kana-rule-list-Ja)
  (skk-reconstruct-rule-tree))

(skk-use-en-signs)
(make-variable-buffer-local 'skk-rule-tree)
(make-variable-buffer-local 'skk-rom-kana-rule-list)
