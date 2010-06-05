(defvar hatena-diary-filetypes
   '(a2ps a65 aap abap abaqus abc abel acedb actionscript ada aflex ahdl
          alsaconf amiga aml ampl ant antlr apache apachestyle arch art asm
          asm68k asmh8300 asn aspperl aspvbs asterisk asteriskvm atlas
          automake ave awk ayacc b baan basic bc bdf bib bindzone blank bst btm
          c calendar catalog cdl cf cfg ch change changelog chaskell cheetah
          chill chordpro cl clean clipper cmake cobol colortest conf config
          context cpp crm crontab cs csc csh csp css cterm ctrlh cupl cuplsim
          cvs cvsrc cweb cynlib cynpp d dcd dcl debchangelog debcontrol
          debsources def desc desktop dictconf dictdconf diff dircolors diva
          django dns docbk docbksgml docbkxml dosbatch dosini dot doxygen
          dracula dsl dtd dtml dylan dylanintr dylanlid ecd edif eiffel elf
          elinks elmfilt erlang eruby esmtprc esqlc esterel eterm eviews exim
          expect exports fasm fdcc fetchmail fgl flexwiki focexec form forth
          fortran foxpro fstab fvwm fvwm2m4 gdb gdmo gedcom gkrellmrc gnuplot
          go gp gpg grads gretl groff groovy group grub gsp gtkrc haskell hb
          help hercules hex hitest hog html htmlcheetah htmldjango htmlm4
          htmlos ia64 icemenu icon idl idlang indent inform initex inittab
          ipfilter ishd iss ist jal jam jargon java javacc javascript jess
          jgraph jproperties jsp kconfig kix kscript kwt lace latte ld ldif
          lex lftp lhaskell libao lifelines lilo limits lisp lite loginaccess
          logindefs logtalk lotos lout lpc lprolog lscript lss lua lynx m4 mail
          mailaliases mailcap make man manconf manual maple masm mason master
          matlab maxima mel mf mgl mgp mib mma mmix modconf model modsim3
          modula2 modula3 monk moo mp mplayerconf mrxvtrc msidl msql mupad mush
          muttrc mysql named nanorc nasm nastran natural ncf netrc netrw
          nosyntax nqc nroff nsis objc objcpp ocaml occam omnimark openroad
          opl ora pamconf papp pascal passwd pcap pccts perl pf pfmain php
          phtml pic pike pilrc pine pinfo plaintex plm plp plsql po pod
          postscr pov povini ppd ppwiz prescribe procmail progress prolog
          protocols psf ptcap purifylog pyrex python qf quake r racc radiance
          ratpoison rc rcs rcslog readline rebol registry remind resolv rexx
          rhelp rib rnc rnoweb robots rpcgen rpl rst rtf ruby samba sas sather
          scala scheme scilab screen sdl sed sendpr sensors services setserial
          sgml sgmldecl sgmllnx sh sicad sieve simula sinda sindacmp sindaout
          sisu skill sl slang slice slpconf slpreg slpspi slrnrc slrnsc sm
          smarty smcl smil smith sml snnsnet snnspat snnsres snobol4 spec
          specman spice splint spup spyce sql sqlanywhere sqlforms sqlinformix
          sqlj sqloracle sqr squid sshconfig sshdconfig st stata stp strace
          sudoers svn syncolor synload syntax sysctl tads tags tak takcmp
          takout tar tasm tcl tcsh terminfo tex texinfo texmf tf tidy tilde
          tli tpp trasys trustees tsalt tsscl tssgm tssop uc udevconf udevperm
          udevrules uil updatedb valgrind vb vera verilog verilogams vgrindefs
          vhdl vim viminfo virata vmasm vrml vsejcl wdiff web webmacro wget
          whitespace winbatch wml wsh wsml wvdial xdefaults xf86conf xhtml
          xinetd xkb xmath xml xmodmap xpm xpm2 xquery xs xsd xslt xxd yacc
          yaml z8a zsh))
(defvar hatena-diary-filetype-alist
      '((cpp . c++) (ocaml . tuareg) (perl . cperl) (tex . latex) (zsh . sh)))
(defvar hatena-diary-major-mode-alist
      (mapcar
       '(lambda (x)
          (let ((ft (or (cdr (assq x hatena-diary-filetype-alist)) x)))
            (cons x (intern (concat (symbol-name ft) "-mode")))))
       hatena-diary-filetypes))
(defun hatena-diary-install-multi-mode ()
  (interactive)
  (let ((m 'text-mode) cache)
    (multi-mode-init m)
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "^>|\\([0-9a-zA-Z]+\\)|$" nil t)
        (let ((ft (intern (match-string 1))) (begin (match-string 0)))
          (unless (memq ft cache)
            (add-to-list 'cache ft)
            (let ((mode (cdr (assq ft hatena-diary-major-mode-alist))))
              (multi-install-chunk-finder
               (concat "^" begin "$") "^||<$"
               (if (functionp mode) mode m)))))))))
