; personal settings
(load "dot/skk-ext")

; use skk server
(setq skk-server-host "localhost")
(setq skk-server-portnum 1179)

(setq skk-dcomp-activate t)
(setq skk-dcomp-multiple-activate t)

(set-face-foreground 'skk-dcomp-multiple-face "black")
(set-face-background 'skk-dcomp-multiple-face "lightgray")
(set-face-foreground 'skk-dcomp-multiple-trailing-face "yellow")
(set-face-background 'skk-dcomp-multiple-selected-face "blue")

; skk-lookup (use dictionary server)
;; (autoload 'skk-lookup-search "skk-lookup")
;; (setq skk-search-prog-list
;;       (append skk-search-prog-list
;;               '(
;;                 (skk-lookup-search)
;;                 )))

; standalone
;; (setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.L")
;; (setq skk-search-prog-list
;;       (append skk-search-prog-list
;;               '(
;;                 (skk-search-jisyo-file
;;                  "/usr/share/skk/SKK-JISYO.pubdic+" 10000)
;;                 (skk-search-jisyo-file
;;                  "/usr/share/skk/SKK-JISYO.jinmei" 10000)
;;                 (skk-search-jisyo-file
;;                  "/usr/share/skk/SKK-JISYO.geo" 10000)
;;                 (skk-search-jisyo-file
;;                  "/usr/share/skk/SKK-JISYO.JIS2" 10000)
;;                 (skk-search-jisyo-file
;;                  "/usr/share/skk/SKK-JISYO.JIS3_4" 10000)
;;                 (skk-search-jisyo-file
;;                  "/usr/share/skk/SKK-JISYO.zipcode" 10000)
;;                 )))
