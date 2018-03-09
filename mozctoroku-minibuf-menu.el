;; -*- Emacs-Lisp -*-
;; mozctoroku-minibuf-menu.el	     mini-buffer $B$K%a%K%e!<$rI=<($9$k(B

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; This program based on:
;; wxg-minibuf-menu.el	     mini-buffer $B$K%a%K%e!<$rI=<($9$k(B
;;
;; Author: Takeshi KATO <kato@mickey.ai.kyutech.ac.jp>
;; Maintainer: Takeshi KATO <kato@mickey.ai.kyutech.ac.jp>
;; Created: January 2001
;; Version: 0.01 Alpha

;; [ $B;H$$J}(B ]
;;
;; (mozctoroku-minibuf-menu prompt itemlist)
;; 
;; prompt $B$K$O%a%K%e!<$N%W%m%s%W%H$NJ8;zNs(B
;; itemlist $B$O%a%K%e!<$N%"%$%F%`$N%j%9%H(B
;;
;; itemlist $B$O0J2<$N$h$&$J%U%)!<%^%C%H$K$J$k(B
;; ("item1" "item2" "item3" ... )
;;
;; $B8=>u$G$O%5%V%a%K%e!<$d!"%G%U%)%k%H$GA*Br$7$F$$$k(B
;; $B9`L\$J$I$rA*$V5!G=$O%5%]!<%H$7$F$$$J$$(B

(provide 'mozctoroku-minibuf-menu)

(defvar mozctoroku-minibuf-menu-subitem-size 5
  "*mini-buffer $B$K0lEY$KI=<($9$k%"%$%F%`$N?t(B")

(defvar mozctoroku-minibuf-menu-setup-hook nil
  "minibuffer $B$K%a%K%e!<$rI=<($9$k;~$N(B hook")
(defvar mozctoroku-minibuf-menu-exit-hook nil
  "minibuffer $B$N%a%K%e!<$+$iH4$1$k$H$-$N(B hook")

(defun mozctoroku-minibuf-menu-create-itemlabel (sub-items)
  "10$B8D0JFb$N%"%$%F%`$N%j%9%H$+$i(B mini-buffer $B$K;XDj$9$kJ8;zNs$r@8@.$9$k(B"
  (let ((label-string nil) (count 0) item item-label)
    (while (and (< count mozctoroku-minibuf-menu-subitem-size)
		sub-items)
      (setq item (car sub-items))
      (setq sub-items (cdr sub-items))
      (setq item-label (format "%d: %s" count item))
      (setq count (1+ count))
      
      (if label-string
	  (setq label-string (format "%s  %s" label-string item-label))
	(setq label-string item-label)))
    label-string))

(defun mozctoroku-minibuf-menu-create-subitem-list (items)
  "items $B$GM?$($i$l$?%"%$%F%`$r(B10$B8DC10L$G6h@Z$C$?O"A[%j%9%H$rJV$9(B.
$B%G!<%?$H$7$F$O<!$N$h$&$J7A$K$J$C$F$$$k(B.
  (n (0 . (\"item00\" \"item01\" \"item02\" \"item03\" \"item04\" ...))
     (1 . (\"item10\" \"item11\" \"item12\" \"item13\" \"item14\" ...))
     ....)
n $B$K$O%5%V%"%$%F%`$N?t$,F~$k(B.$B%"%$%F%`$,(B23$B8D$"$k$J$i$P(B 3 $B$,F~$k(B."
  (let ((submenu-alist nil)
	(submenu nil)
	(submenu-count 0)
	(count 0)
	item)
    (while items
      (setq item (car items))
      (setq items (cdr items))
      (setq count (1+ count))
      (setq submenu (cons item submenu))
      
      (if (= count mozctoroku-minibuf-menu-subitem-size)
	  (progn
	    (setq submenu-alist (cons (cons submenu-count (reverse submenu))
				      submenu-alist))
	    (setq submenu-count (1+ submenu-count))
	    (setq submenu nil)
	    (setq count 0))))
    (if submenu
	(progn
	  (setq submenu-alist (cons (cons submenu-count (reverse submenu))
				    submenu-alist))
	  (setq submenu-count (1+ submenu-count))))
    (cons submenu-count (reverse submenu-alist))))

(defun mozctoroku-minibuf-menu-erase-buffer ()
  "minibuffer $B$G$NF~NO$NFbMF$r:o=|(B"
  (if (>= emacs-major-version 21)
      (delete-region (minibuffer-prompt-end) (point-max))
    (erase-buffer)))

(defun mozctoroku-minibuf-menu-disp-subitem (subitem current total)
  "SUBITEM $B$G<($5$l$F$$$k%"%$%F%`$r(B mini-buffer $B$KI=<((B"
  (mozctoroku-minibuf-menu-erase-buffer)
  (insert (mozctoroku-minibuf-menu-create-itemlabel subitem))
  (insert (format "\t(%d/%d)" (1+ current) total))
  (mozctoroku-minibuf-menu-seek-head-item))

(defun mozctoroku-minibuf-menu-seek-next-subitem (current subitem-alist)
  "subitem-alist $B$K4^$^$l$k%5%V%"%$%F%`$G!"(Bcurrent $B$N<!$N%5%V%"%$%F%`$K(B
$B%a%K%e!<$r@Z$jBX$($k(B. $B$3$N(Bfunction$B$NCM$O<!$N(B current $B$NCM$K$J$k(B"
  (let (subitem)
    (setq current (1+ current))
    (if (>= current (car subitem-alist))
	(setq current 0))
    (setq subitem (cdr (assoc current (cdr subitem-alist))))
    (mozctoroku-minibuf-menu-disp-subitem subitem current (car subitem-alist)))
  current)

(defun mozctoroku-minibuf-menu-seek-prev-subitem (current subitem-alist)
  "subitem-alist $B$K4^$^$l$k%5%V%"%$%F%`$G!"(Bcurrent $B$NA0$N%5%V%"%$%F%`$K(B
$B%a%K%e!<$r@Z$jBX$($k(B. $B$3$N(Bfunction$B$NCM$OA0$N(B current $B$NCM$K$J$k(B"
  (let (subitem)
    (setq current (1- current))
    (if (< current 0)
	(setq current (1- (car subitem-alist))))
    (setq subitem (cdr (assoc current (cdr subitem-alist))))
    (mozctoroku-minibuf-menu-disp-subitem subitem current (car subitem-alist)))
  current)

(defun mozctoroku-minibuf-menu-seek-head-item ()
  "$B8=:_I=<($7$F$$$k%"%$%F%`$N@hF,$K%+!<%=%k$r0\F0(B"
  (interactive)
  (if (>= emacs-major-version 21)
      (goto-char (minibuffer-prompt-end))
    (goto-char (point-min))))

(defun mozctoroku-minibuf-menu-seek-tail-item ()
  "$B8=:_I=<($7$F$$$k%"%$%F%`$NKvHx$K%+!<%=%k$r0\F0(B"
  (interactive)
  (goto-char (point-max))
  (re-search-backward "\\([0-9]\\): [^ \t]+")
  (goto-char (match-beginning 1)))

(defun mozctoroku-minibuf-menu-seek-next-item ()
  "$B8=:_A*Br$7$F$$$k%"%$%F%`$N<!$N%"%$%F%`$K%+!<%=%k$r9g$o$;$k(B"
  (interactive)
  ;; $B:#!"$$$k0LCV$+$i%^%C%A;XDj$7$F$$$k$+$r%A%'%C%/(B ($B$3$l$O$G$-$k$O$:(B)
  (if (looking-at "\\([0-9]: [^ \t]+\\)")
      (goto-char (match-end 1))) ;; $B%^%C%A;XDj$l$P!"$=$l$rL5;k$9$k(B
  ;; $B<!$NMWAG$r8!:w$7$F0\F0(B
  (if (not (re-search-forward "\\([0-9]\\): [^ \t]+" (point-max) t))
      (mozctoroku-minibuf-menu-seek-head-item)
    (goto-char (match-beginning 1))))

(defun mozctoroku-minibuf-menu-seek-prev-item ()
  "$B8=:_A*Br$7$F$$$k%"%$%F%`$NA0$N%"%$%F%`$K%+!<%=%k$r9g$o$;$k(B"
  (interactive)
  (if (equal (point) (point-min))
      (mozctoroku-minibuf-menu-seek-tail-item)
    (if (not (re-search-backward "\\([0-9]\\): [^ \t]+" (point-min) t))
	(mozctoroku-minibuf-menu-seek-tail-item)
      (goto-char (match-beginning 1)))))

(defun mozctoroku-minibuf-menu-seek-n-item (n max)
  "$B%+!<%=%k$N0LCV$r(B N $BHVL\$N%"%$%F%`$K9g$o$;$k!#(BMAX $B$K$O(Bsubitem$B$N?t$,F~$k(B"
  (if (not (>= n max))
      (progn
	(mozctoroku-minibuf-menu-seek-head-item) ;; $B@hF,$K0\F0(B
	(while (> n 0)
	  (setq n (1- n))
	  (mozctoroku-minibuf-menu-seek-next-item)))))

(defun mozctoroku-minibuf-menu-select-item ()
  "$B8=:_$N%+!<%=%k$N0LCV$N%"%$%F%`$NJ8;zNs$rA*Br$5$l$?%"%$%F%`$H$7$F(B
mini-buffer $B$N=hM}$r=*N;$9$k(B."
  (interactive)
  (if (not (looking-at "\\([0-9]: [^ \t]+\\)"))
      (if (not (re-search-backward "[0-9]: [^ \t]+" (point-min) t))
	  (mozctoroku-minibuf-menu-seek-head-item)))
  (let (str)
    (looking-at "[0-9]: \\([^ \t]+\\)")
    (setq str (buffer-substring (match-beginning 1)
				(match-end 1)))
    (mozctoroku-minibuf-menu-erase-buffer)
    (insert str))
  (exit-minibuffer))

(defun mozctoroku-minibuf-menu (prompt items)
  "mini-buffer $B$K%a%K%e!<$rI=<($9$k(B"
  (let (mozctoroku-minibuf-menu-map
	subitem-curent subitem-alist
	initial-label item)
    (setq prompt (format "%s: " prompt))
    (setq menus-string nil)
    
    ;; $B%a%K%e!<MQ$N(Bkeymap$B$r:n@.(B
    (setq mozctoroku-minibuf-menu-map (copy-keymap minibuffer-local-map))
    (suppress-keymap mozctoroku-minibuf-menu-map)
    (define-key mozctoroku-minibuf-menu-map " "
      (lambda ()
	(interactive)
	(setq subitem-current
	      (mozctoroku-minibuf-menu-seek-next-subitem
	       subitem-current
	       subitem-alist))))
    (define-key mozctoroku-minibuf-menu-map [down]
      (lambda ()
	(interactive)
	(setq subitem-current
	      (mozctoroku-minibuf-menu-seek-next-subitem
	       subitem-current
	       subitem-alist))))
    (define-key mozctoroku-minibuf-menu-map "\C-n"
      (lambda ()
	(interactive)
	(setq subitem-current
	      (mozctoroku-minibuf-menu-seek-next-subitem
	       subitem-current
	       subitem-alist))))
    (define-key mozctoroku-minibuf-menu-map [up]
      (lambda ()
	(interactive)
	(setq subitem-current
	      (mozctoroku-minibuf-menu-seek-prev-subitem
	       subitem-current
	       subitem-alist))))
    (define-key mozctoroku-minibuf-menu-map "\C-p"
      (lambda ()
	(interactive)
	(setq subitem-current
	      (mozctoroku-minibuf-menu-seek-prev-subitem
	       subitem-current
	       subitem-alist))))
    (define-key mozctoroku-minibuf-menu-map "\C-a"
      'mozctoroku-minibuf-menu-seek-head-item)
    (define-key mozctoroku-minibuf-menu-map "\C-e"
      'mozctoroku-minibuf-menu-seek-tail-item)
    (define-key mozctoroku-minibuf-menu-map "\C-f"
      'mozctoroku-minibuf-menu-seek-next-item)
    (define-key mozctoroku-minibuf-menu-map "\C-i"
      'mozctoroku-minibuf-menu-seek-next-item)
    (define-key mozctoroku-minibuf-menu-map [tab]
      'mozctoroku-minibuf-menu-seek-next-item)
    (define-key mozctoroku-minibuf-menu-map [right]
      'mozctoroku-minibuf-menu-seek-next-item)
    (define-key mozctoroku-minibuf-menu-map "\C-b"
      'mozctoroku-minibuf-menu-seek-prev-item)
    (define-key mozctoroku-minibuf-menu-map [backtab]
      'mozctoroku-minibuf-menu-seek-prev-item)
    (define-key mozctoroku-minibuf-menu-map [left]
      'mozctoroku-minibuf-menu-seek-prev-item)
    (define-key mozctoroku-minibuf-menu-map "\C-j"
      'mozctoroku-minibuf-menu-select-item)
    (define-key mozctoroku-minibuf-menu-map "\C-m"
      'mozctoroku-minibuf-menu-select-item)
    (substitute-key-definition 'forward-char
			       'mozctoroku-minibuf-menu-seek-next-item
			       mozctoroku-minibuf-menu-map)
    (substitute-key-definition 'exit-minibuffer
			       'mozctoroku-minibuf-menu-select-item
			       mozctoroku-minibuf-menu-map)
    (let ((n 0))
      (while (< n mozctoroku-minibuf-menu-subitem-size)
	(define-key mozctoroku-minibuf-menu-map
	  (format "%c" (+ n ?0))
	  (lambda ()
	    (interactive)
	    (let ((subitem (cdr (assoc subitem-current
				       (cdr subitem-alist))))
		  size)
	      (setq size (length subitem))
	      (mozctoroku-minibuf-menu-seek-n-item
	       (- last-command-event ?0) size))))
	(setq n (1+ n))))
    
    (setq subitem-current 0)
    (setq subitem-alist (mozctoroku-minibuf-menu-create-subitem-list items))
    
    (setq initial-label (mozctoroku-minibuf-menu-create-itemlabel
			 (cdr (assoc subitem-current
				     (cdr subitem-alist)))))
    (setq initial-label
	  (concat initial-label
		  (format "\t(%d/%d)" (1+ subitem-current)
			  (car subitem-alist))))
    (run-hooks 'mozctoroku-minibuf-menu-setup-hook)
    (setq item (read-from-minibuffer prompt (cons initial-label 0)
				     mozctoroku-minibuf-menu-map))
    (run-hooks 'mozctoroku-minibuf-menu-exit-hook)
    item))
    
