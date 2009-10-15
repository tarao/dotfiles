; personal settings
(load "dot/skk-ext")

; use skk server
(setq skk-server-host "localhost")
(setq skk-server-portnum 1178)

; use skk-look
;; (setq skk-use-look t)
;; (setq skk-search-prog-list
;;        (append skk-search-prog-list
;;                (list
;;                 '(skk-lookup-search))))

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
