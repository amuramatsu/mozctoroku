;; -*- Emacs-Lisp -*-
;; mozctoroku.el          mozc $B%f!<%6<-=qEPO?$r9T$&(B

;; $BK\%W%m%0%i%`$O%U%j!<!&%=%U%H%&%'%"$G$9!#$"$J$?$O!"(BFree Software
;; Foundation $B$,8xI=$7$?(B GNU $B0lHG8xMQ;HMQ5vBz$N!V%P!<%8%g%s(B2$B!W0?$O$=$l(B
;; $B0J9_$N3F%P!<%8%g%s$NCf$+$i$$$:$l$+$rA*Br$7!"$=$N%P!<%8%g%s$,Dj$a$k(B
;; $B>r9`$K$7$?$,$C$FK\%W%m%0%i%`$r:FHRI[Kt$OJQ99$9$k$3$H$,=PMh$^$9!#(B
;;
;; $BK\%W%m%0%i%`$OM-MQ$H$O;W$$$^$9$,!"HRI[$K$"$?$C$F$O!";T>l@-5Z$SFCDj(B
;; $BL\E*E,9g@-$K$D$$$F$N0EL[$NJ]>Z$r4^$a$F!"$$$+$J$kJ]>Z$b9T$J$$$^$;$s!#(B
;; $B>\:Y$K$D$$$F$O(B GNU $B0lHL8xMQ;HMQ5vBz=q$r$*FI$_2<$5$$!#(B
;;
;; $B$"$J$?$O!"K\%W%m%0%i%`$H0l=o$K(B GNU $B0lHL8xMQ;HMQ5vBz$N<L$7$r<u$1$H$C(B
;; $B$F$$$k$O$:$G$9!#$=$&$G$J$$>l9g$O!"(BFree Software Foundation, Inc.,
;; 675 Mass Ave, Cambridge, MA 02139, USA $B$X<j;f$r=q$$$F2<$5$$!#(B

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; This program based on:
;; wxgtoroku.el           wxg $B%f!<%6<-=qEPO?$r9T$&(B
;;
;; Author: Takeshi KATO <kato@mickey.ai.kyutech.ac.jp>
;; Maintainer: Takeshi KATO <kato@mickey.ai.kyutech.ac.jp>
;; Created: January 2001
;; Version: 0.02.13 beta 5

;;
;;
;;$B!}(B $B;H$$J}(B
;;
;;  [ .emacs $B$G$N@_Dj(B ]
;; 
;;        (autoload 'mozctoroku-toroku "mozctoroku"
;;                  "MOZC $B%f!<%6<-=qEPO?(B" t nil)
;;
;;  [ $BA`:nJ}K!(B ]
;;
;;     $B;H$$J}$OB?J,%N%j$G$o$+$k$H$O;W$$$^$9$,0l1~=q$$$F$*$-$^$9!#(B
;;     $B$^$:!"(BM-x mozctoroku-toroku $B$H<B9T$7$^$9!#(B
;;
;;     $B<!$K!"<-=q$KEPO?$9$kC18l$rJ9$$$F$-$^$9$N$G!"F~NO$7$F$/$@$5$$!#(B
;;     $B$=$N$"$H$KC18l$NFI$_$rJ9$$$F$-$^$9$N$G!"$3$l$bF~NO$7$^$9!#(B
;;     $B$3$N#2$D$ODL>o$N%_%K%P%C%U%!$GJ8;zNs$rF~NO$9$k$N$HF1$8%-!<$,(B
;;     $B;H$($k$O$:$G$9!#(B
;;
;;     $B:G8e$K<-=q$KEPO?$9$k:]$NIJ;l$rJ9$$$F$-$^$9$N$G!"A*Br$7$F$/$@$5$$!#(B
;;     $BIJ;l$NA*Br$K$O0J2<$N%-!<$,;H$($^$9!#(B
;;
;;         C-g     $B%-%c%s%;%k(B
;;         Enter   $B9`L\$rA*Br(B
;;         C-a     $B:8C<$N9`L\$X0\F0(B
;;         C-e     $B1&C<$N9`L\$X0\F0(B
;;         C-b     $B0l$D:8$N9`L\$K0\F0(B
;;         C-f     $B0l$D1&$N9`L\$K0\F0(B
;;         C-p     $BA0$N%a%K%e!<$K0\F0(B
;;         C-n     $B<!$N%a%K%e!<$K0\F0(B
;;        
;;     $B:G8e$N#2$D$O0UL#$,$o$+$j$K$/$$$H$O;W$&$N$G$9$,!"%a%K%e!<$N(B
;;     $BI=<($O#4$D$:$D$K$J$C$F$^$9!#$3$l$O$G$-$k8B$j0lEY$KI=<($G$-$k(B
;;     $B$h$&$K$7$?$+$C$?$+$i$J$s$G$9!#$=$l$G9`L\?t$,#5$D$r$3$($k>l9g$K(B
;;     $B9`L\$r@Z$jBX$($k$N$K:G8e$N#2$D$rMxMQ$7$^$9!#(B
;;
;;     $B$"$H!"?t;z%-!<$r2!$9$3$H$G!"%"%$%F%`$rA*Br(B($B<B:]$K$O(BEnter$B$r(B
;;     $B2!$9I,MW$,$"$j$^$9(B)$B$9$k$3$H$,$G$-$^$9!#(B
;;

(require 'mozctoroku-minibuf-menu)

;; $B%P!<%8%g%s(B
(defconst mozctoroku-version "0.0.1 alpha")
(defun mozctoroku-version ()
  (interactive)
  (message "mozctoroku version %s" mozctoroku-version))

;; mozc-dict $B$N%3%^%s%I$N>l=j$r;XDj(B
(defvar mozctoroku-mozc-dict-command nil
  "mozc-dict $B$N%3%^%s%IL>$r;XDj$9$k!#(Bnil $B$N>l9g$O(B mozc-dict $B$r;H$&!#(B
$BNc(B:
\(setq mozctoroku-mozc-dict-command \"/usr/bin/mozc-dict\")")

(defvar mozctoroku-logfile nil
  "*$B<-=qEPO?$N:]$K!"EPO?$7$?C18l$r5-O?$9$k%m%0%U%!%$%k$r;XDj$9$k!#(B
nil $B$N>l9g$K$O%m%0$O5-O?$5$l$J$$(B")
(defvar mozctoroku-word-at-point nil
  "*$B<-=qEPO?$r9T$J$&:]$K%+!<%=%k$N0LCV$NC18l$r<h$j=P$9$+!)$r7h$a$k!#(B
$B%U%i%0!#(Bnon-nil $B$J$i$P!"(Bword-at-point$B$r;H$$!"%+!<%=%k0LCV$NC18l$r<h$j=P$9(B")

(defun mozctoroku-without-newline (beg end)
  "$B%j!<%8%g%sFb$K2~9T%3!<%I$r4^$`$+H=Dj$9$k(B"
  (interactive "r")
  (if (not (and (integer-or-marker-p beg)
		(integer-or-marker-p end)))
      (error "hogehogehogehoge"))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (if (re-search-forward "[\r\n]" nil t)
	  nil
	t))))

;; XEmacs $B0J30$G$O(B region-exists-p $B$,;H$($J$$$N$G!"(B
;; region-exists-p $B$rDj5A(B
(defun mozctoroku-region-exists-p ()
  (if (featurep 'xemacs)
      (region-exists-p)
    mark-active))

;; $BIJ;lL>A*BrMQ$N%"%$%F%`%j%9%H(B
(defconst mozctoroku-hinsi-list
  '(("$BL>;l(B" (("$BIaDLL>;l(B" ("$BL>;l(B"
			  "$BL>;l7AF0(B"
			  "$BL>;l%5JQ(B"))
	     ("$B8GM-L>;l(B" (("$B@+L>(B" ("$B@+(B" "$BL>(B" "$B?ML>(B"))
			  "$BCOL>(B"
			  "$BAH?%L>(B"
			  "$B8GM-L>;l(B"))
	     "$BC;=L$h$_(B"))
    ("$BF0;l(B" (("$BF0;l8^CJ(B" ("$B%o9T8^CJ(B"
			  "$B%+9T8^CJ(B"
			  "$B%59T8^CJ(B"
			  "$B%?9T8^CJ(B"
			  "$B%J9T8^CJ(B"
			  "$B%^9T8^CJ(B"
			  "$B%i9T8^CJ(B"
			  "$B%,9T8^CJ(B"
			  "$B%P9T8^CJ(B"))
	     "$BF0;l0lCJ(B"
	     ("$BJQ3J3hMQ(B" ("$BF0;l%+JQ(B"
			  "$BF0;l%5JQ(B"
			  "$BF0;l%6JQ(B")
	     ("$BFC<l3hMQ(B" ("$BF0;l%iJQ(B"
			  "$B%O9T;MCJ(B")))))
    ("$B7AMF;l(B" ("$B7AMF;l(B" "$B7AMFF0;l(B"))
    ("$B=$>~8l(B" ("$BI{;l(B" "$BO"BN;l(B"))
    ("$BFHN)8l(B" ("$B5-9f(B" "$B4iJ8;z(B" "$B46F0;l(B" "$BFHN)8l(B" "$BC14A;z(B" "$B?t(B" "Alphabet"))
    ("$B@\F,Hx8l(B" ("$B@\F,8l(B" "$B=u?t;l(B" "$B@\Hx0lHL(B" "$B@\Hx?ML>(B" "$B@\HxCOL>(B"))
    ("$B$=$NB>(B" ("$B@\B3;l(B" "$B=*=u;l(B" "$B6gFIE@(B" "$BM^@)C18l(B")))
  "$BIJ;lA*Br$N%a%K%e!<9`L\$N%j%9%HDj5A(B")

(defconst mozctoroku-mozc-hinsi-alist
  '(("Alphabet" . "$B%"%k%U%!%Y%C%H(B")
    ("$B7AMFF0;l(B" . "$BL>;l7AF0(B")
    ("$BC14A;z(B" . "$BL>;l(B")
    ("$B%o9T8^CJ(B" . "$BF0;l%o9T8^CJ(B")
    ("$B%+9T8^CJ(B" . "$BF0;l%+9T8^CJ(B")
    ("$B%59T8^CJ(B" . "$BF0;l%59T8^CJ(B")
    ("$B%?9T8^CJ(B" . "$BF0;l%?9T8^CJ(B")
    ("$B%J9T8^CJ(B" . "$BF0;l%J9T8^CJ(B")
    ("$B%^9T8^CJ(B" . "$BF0;l%^9T8^CJ(B")
    ("$B%i9T8^CJ(B" . "$BF0;l%i9T8^CJ(B")
    ("$B%,9T8^CJ(B" . "$BF0;l%,9T8^CJ(B")
    ("$B%P9T8^CJ(B" . "$BF0;l%P9T8^CJ(B")
    ("$B%O9T;MCJ(B" . "$BF0;l%O9T;MCJ(B")))

(defvar mozctoroku-back-menu-string "$BLa$k(B")

(defun mozctoroku-create-menu-list (menu-list &optional submenu)
  "mozctoroku-hinsi-list $B$N$h$&$J%j%9%H$+$i%a%K%e!<$r$D$/$j=P$9(B"
  (let (item (created-menu nil))
    (while menu-list
      (setq item (car menu-list))
      (setq menu-list (cdr menu-list))
      ;; $B%5%V%a%K%e!<$r;}$C$F$$$k>l9g$O%j%9%H$K$J$k$N$G!"(B
      ;; $B$=$N>l9g$O@hF,MWAG$r(B item $B$H$9$k(B
      (if (listp item)
	  (setq item (car item)))
      
      (if created-menu
	  (setq created-menu (append created-menu (cons item nil)))
	(setq created-menu (cons item nil))))

    (if submenu
	(cons mozctoroku-back-menu-string created-menu)
      created-menu)))

(defun mozctoroku-hinsi-menu (label hlist submenu-flag)
  "$B0z?t(B HLIST $B$+$iIJ;l$r<h$j=P$9(B"
  (let ((current-menu (mozctoroku-create-menu-list hlist submenu-flag))
	(loop-flag t)
	selected item hinsi)
    (while loop-flag
      (setq loop-flag nil)
      (setq selected (mozctoroku-minibuf-menu label current-menu))
      (setq item (assoc selected hlist))
      (if (equal selected mozctoroku-back-menu-string)
	  ;; $BLa$k$,A*Br$5$l$?(B
	  (setq hinsi nil)
	(if item
	    (progn ;; $B%5%V%a%K%e!<$,B8:_$9$k(B
	      (setq hinsi
		    (mozctoroku-hinsi-menu selected (car (cdr item)) t))
	      (if (not hinsi) ;; $B%5%V%a%K%e!<$GLa$k$,A*Br$5$l$?(B
		  (setq loop-flag t)))
	  ;; $B%5%V%a%K%e!<$,B8:_$7$J$$(B
	  (setq hinsi selected))))
    hinsi))

(defun mozctoroku-userdic-menu (label dlist)
  "$B0z?t(B DLIST $B$+$i<-=q$r<h$j=P$9(B"
  (let (current-menu userdic)
    (setq current-menu (mozctoroku-create-menu-list dlist nil))
    (setq userdic (mozctoroku-minibuf-menu label current-menu))
    userdic))

(defvar mozctoroku-setup-read-japanese-string-hook nil)
(defvar mozctoroku-exit-read-japanese-string-hook nil)

(defun mozctoroku-read-japanese-string
  (prompt &optional initial history default mehtod)
  "minibuffer $B$GF|K\8l$NJ8;zNs$rF~NO$5$;$k!#(B
INITIAL $B$O%f!<%6$,F~NO$9$kA0$+$iF~NO$5$l$F$$$k%G%U%)%k%HJ8;zNs!#(B
HISOTRY $B$OF~NOMzNr$H$7$F;H$&JQ?t$r;XDj$9$k!#(B"
  (let (input
	(enter-hook minibuffer-setup-hook)
	(exit-hook minibuffer-exit-hook))
    (unwind-protect
	(progn
	  ;; $B%_%K%P%C%U%!$GF|K\8lF~NO$r%G%U%)%k%H$K(B
	  (add-hook 'minibuffer-setup-hook
		    '(lambda ()
		       (activate-input-method default-input-method)))
	  (add-hook 'minibuffer-exit-hook
		    '(lambda ()
		       (inactivate-input-method)))
	  ;; hook $B$N<B9T(B
	  (run-hooks 'mozctoroku-setup-read-japanese-string-hook)
	  (setq input
		(read-from-minibuffer prompt initial nil nil history)))
      ;; $B8e;OKv(B
      (run-hooks 'mozctoroku-exit-read-japanese-string-hook)
      (setq minibuffer-setup-hook enter-hook)
      (setq minibuffer-exit-hook exit-hook))
    input))

(defun mozctoroku-toroku ()
  "$BC18l$r(Bmozc$B$N%f!<%6<-=q$KEPO?$9$k(B"
  (interactive)
  (cond ((mozctoroku-region-exists-p) ;; $B%j!<%8%g%s$,;XDj$5$l$F$$$k(B
	 (let ((start (mark))
	       (end (point)) tmp)
	   ;; start < end $B$H$J$k$h$&$K=$@5(B
	   (if (> start end)
	       (mozctoroku-toroku-region end start)
	   (mozctoroku-toroku-region start end))))
	(mozctoroku-word-at-point     ;; word-at-point $B$K$h$kC18l@Z$j=P$7(B
	 (mozctoroku-word-at-point))
	(t                            ;; $B$=$l0J30(B
	 (mozctoroku-session ""))))

(defun mozctoroku-word-at-point ()
  "$B%+!<%=%k$N0LCV$NC18l$rEPO?$9$k(B"
  (interactive)
  (require 'thingatpt)
  (let ((word-at-point (word-at-point)))
    (if word-at-point
	(setq word-at-point (current-word)))
    (if word-at-point
	(progn
	  (set-text-properties 0 (length word-at-point) nil word-at-point)
	  (mozctoroku-session word-at-point))
      (mozctoroku-session ""))))

(defun mozctoroku-toroku-region (start end)
  "$BNN0h$r(Bmozc$B$N%f!<%6<-=q$KEPO?$9$k(B"
  (interactive "r")
  ;; start < end $B$K$J$k$h$&$K(B
  (if (> start end)
      (progn
	(setq end (- start end))
	(setq start (- start end))
	(setq end (+ end start))))
  ;; $BEPO?=hM}(B
  (if (mozctoroku-without-newline start end)
      (let ((toroku-word (buffer-substring-no-properties start end)))
	(mozctoroku-session toroku-word))
    (mozctoroku-session "")))

;; mozctoroku-session $B$+$i$N$_8F$S=P$5$l$k4X?t72(B
(defun mozctoroku-read-tango (toroku-word)
  "$BEPO?$9$kC18l$rFI$_9~$_!"$=$l$rLa$94X?t!#(B
TOROKU-WORD $B$O%_%K%P%C%U%!$GEPO?$9$kC18l$rF~NO$9$k:]$N%G%U%)%k%H(B
$BJ8;zNs$H$J$k!#(B"
  (let ((word ""))
    (while (equal word "")
      (setq word
	    (mozctoroku-read-japanese-string
	     "$BC18l(B? "
	     toroku-word 'mozctoroku-tango-str-history))
      (when (equal word "")
	(message "$BEPO?C18l$rF~NO$7$F$/$@$5$$(B.")
	(sit-for 1)))
    word))
(defun mozctoroku-read-yomi (toroku-word &optional yomi)
  "$BEPO?$9$kC18l$NFI$_$rFI$_9~$_!"$=$l$rLa$94X?t!#(B"
  (let ((word ""))
    (while (equal word "")
      (setq word
	    (mozctoroku-read-japanese-string
	     (format "$BC18l(B[%s] $BFI$_(B? " toroku-word)
	      yomi 'mozctoroku-yomi-str-history))
      (when (equal word "")
	(message "$BFI$_$rF~NO$7$F$/$@$5$$(B.")
	(sit-for 1)))
    word))
;; end

(defvar mozctoroku-tango-str-history nil
  "$BEPO?;~$NC18l$NMzNr$r3JG<$9$kJQ?t!#(B")
(defvar mozctoroku-yomi-str-history nil
  "$BEPO?;~$NFI$_$NMzNr$r3JG<$9$kJQ?t!#(B")

;; mozctoroku-session $B$+$iMxMQ$9$k(B read-japanse-string-hook
(defun mozctoroku-session-setup-read-string-hook ()
  "minibuffer $BCf$N2>L>4A;zJQ49$N(B toggle $B$G(B modeline $B$NI=<($rJQ$($J$$$h$&$K(B"
  (setq display-minibuffer-mode-in-minibuffer t))
(defun mozctoroku-session-exit-read-string-hook ()
  "setup-hook $B$G9T$J$C$?@_Dj$r85$KLa$9(B"
  (setq display-minibuffer-mode-in-minibuffer nil))

(defun mozctoroku-session (toroku-word)
  "mozctoroku-toroku, mozctoroku-toroku-region $B$K6&DL$NEPO?=hM}(B"
  (interactive)
  (let ((old-setup-hook mozctoroku-setup-read-japanese-string-hook)
	(old-exit-hook mozctoroku-exit-read-japanese-string-hook)
	(old-minibuf-setup-hook mozctoroku-minibuf-menu-setup-hook)
	(old-minibuf-exit-hook mozctoroku-minibuf-menu-exit-hook)
	word yomi hinsi userdic)
    ;; $BC18lEPO?$KI,MW$J>pJs$NEPO?(B
    (unwind-protect
	(progn
	  ;; $B%U%C%/$rMQ$$$FEPO?Cf$N(B mode-line $B$NI=<($r8GDj(B
	  (add-hook 'mozctoroku-setup-read-japanese-string-hook
		    'mozctoroku-session-setup-read-string-hook)
	  (add-hook 'mozctoroku-exit-read-japanese-string-hook
		    'mozctoroku-session-exit-read-string-hook)
	  (add-hook 'mozctoroku-minibuf-menu-setup-hook
		    'mozctoroku-session-setup-read-string-hook)
	  (add-hook 'mozctoroku-minibuf-menu-exit-hook
		    'mozctoroku-session-exit-read-string-hook)
	  
	  ;; $B<B:]$NC18l$NFI$_9~$_(B
	  (setq word (mozctoroku-read-tango toroku-word))
	  (setq yomi (mozctoroku-read-yomi word))
	  
	  (setq hinsi
		(mozctoroku-hinsi-menu "$BIJ;l(B" mozctoroku-hinsi-list nil)))
      ;; $B%U%C%/$N@_Dj$rLa$9(B
      (setq mozctoroku-setup-read-japanese-string-hook old-setup-hook)
      (setq mozctoroku-exit-read-japanese-string-hook old-exit-hook)
      (setq mozctoroku-minibuf-menu-setup-hook old-minibuf-setup-hook)
      (setq mozctoroku-minibuf-menu-exit-hook old-minibuf-exit-hook))
    
    ;; $B<-=q$X$NEPO?(B
    (mozctoroku-process word yomi hinsi)))

(defun mozctoroku-log (word yomi hinsi)
  "$B%m%0%U%!%$%k$KEPO?$9$kC18l$r5-O?$9$k(B"
  (let ((mozctoroku-log-file (expand-file-name mozctoroku-logfile))
	(mozctoroku-log-buffer ".mozctoroku-log")
	(coding-system-for-read 'utf-8)
	(coding-system-for-write 'utf-8)
	buf)
    (save-excursion
      (setq buf (get-buffer-create mozctoroku-log-buffer))
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; $B0JA0$N%m%0$NFbMF$r%3%T!<(B
      (if (file-exists-p mozctoroku-log-file)
	  (insert-file mozctoroku-log-file))
      ;; $B:G8e$K%m%0$rDI2C(B
      (goto-char (point-max))
      (insert (format "%s\t\"%s\":%s\n" yomi word hinsi))
      (write-file mozctoroku-logfile)
      (kill-buffer buf))))

(defun mozctoroku-process (word yomi hinsi)
  "$B<B:]$NEPO?:n6H$r9T$J$&(B"
  (let ((mozc-dict (or mozctoroku-mozc-dict-command "mozc-dict")))
    ;; $B%m%0%U%!%$%k$X5-O?(B
    (if mozctoroku-logfile
	(mozctoroku-log word yomi hinsi))
    (let ((coding-system-for-read 'utf-8)
	  (coding-system-for-write 'utf-8)
	  (xhinsi (assoc hinsi mozctoroku-mozc-hinsi-alist)))
      (setq xhinsi (if xhinsi (cdr xhinsi) hinsi))
      (call-process mozc-dict nil nil nil "-s" yomi word xhinsi))
  (message (format "%s(%s) $B$rEPO?$7$^$7$?!#(B" word yomi))))

(provide 'mozctoroku)
;; end of mozctoroku
