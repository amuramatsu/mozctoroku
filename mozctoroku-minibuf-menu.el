;; -*- Emacs-Lisp -*-
;; mozctoroku-minibuf-menu.el	     mini-buffer にメニューを表示する

;; 本プログラムはフリー・ソフトウェアです。あなたは、Free Software
;; Foundation が公表した GNU 一版公用使用許諾の「バージョン2」或はそれ
;; 以降の各バージョンの中からいずれかを選択し、そのバージョンが定める
;; 条項にしたがって本プログラムを再頒布又は変更することが出来ます。
;;
;; 本プログラムは有用とは思いますが、頒布にあたっては、市場性及び特定
;; 目的適合性についての暗黙の保証を含めて、いかなる保証も行ないません。
;; 詳細については GNU 一般公用使用許諾書をお読み下さい。
;;
;; あなたは、本プログラムと一緒に GNU 一般公用使用許諾の写しを受けとっ
;; ているはずです。そうでない場合は、Free Software Foundation, Inc.,
;; 675 Mass Ave, Cambridge, MA 02139, USA へ手紙を書いて下さい。

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
;; wxg-minibuf-menu.el	     mini-buffer にメニューを表示する
;;
;; Author: Takeshi KATO <kato@mickey.ai.kyutech.ac.jp>
;; Maintainer: Takeshi KATO <kato@mickey.ai.kyutech.ac.jp>
;; Created: January 2001
;; Version: 0.01 Alpha

;; [ 使い方 ]
;;
;; (mozctoroku-minibuf-menu prompt itemlist)
;; 
;; prompt にはメニューのプロンプトの文字列
;; itemlist はメニューのアイテムのリスト
;;
;; itemlist は以下のようなフォーマットになる
;; ("item1" "item2" "item3" ... )
;;
;; 現状ではサブメニューや、デフォルトで選択している
;; 項目などを選ぶ機能はサポートしていない

(provide 'mozctoroku-minibuf-menu)

(defvar mozctoroku-minibuf-menu-subitem-size 5
  "*mini-buffer に一度に表示するアイテムの数")

(defvar mozctoroku-minibuf-menu-setup-hook nil
  "minibuffer にメニューを表示する時の hook")
(defvar mozctoroku-minibuf-menu-exit-hook nil
  "minibuffer のメニューから抜けるときの hook")

(defun mozctoroku-minibuf-menu-create-itemlabel (sub-items)
  "10個以内のアイテムのリストから mini-buffer に指定する文字列を生成する"
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
  "items で与えられたアイテムを10個単位で区切った連想リストを返す.
データとしては次のような形になっている.
  (n (0 . (\"item00\" \"item01\" \"item02\" \"item03\" \"item04\" ...))
     (1 . (\"item10\" \"item11\" \"item12\" \"item13\" \"item14\" ...))
     ....)
n にはサブアイテムの数が入る.アイテムが23個あるならば 3 が入る."
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
  "minibuffer での入力の内容を削除"
  (if (>= emacs-major-version 21)
      (delete-region (minibuffer-prompt-end) (point-max))
    (erase-buffer)))

(defun mozctoroku-minibuf-menu-disp-subitem (subitem current total)
  "SUBITEM で示されているアイテムを mini-buffer に表示"
  (mozctoroku-minibuf-menu-erase-buffer)
  (insert (mozctoroku-minibuf-menu-create-itemlabel subitem))
  (insert (format "\t(%d/%d)" (1+ current) total))
  (mozctoroku-minibuf-menu-seek-head-item))

(defun mozctoroku-minibuf-menu-seek-next-subitem (current subitem-alist)
  "subitem-alist に含まれるサブアイテムで、current の次のサブアイテムに
メニューを切り替える. このfunctionの値は次の current の値になる"
  (let (subitem)
    (setq current (1+ current))
    (if (>= current (car subitem-alist))
	(setq current 0))
    (setq subitem (cdr (assoc current (cdr subitem-alist))))
    (mozctoroku-minibuf-menu-disp-subitem subitem current (car subitem-alist)))
  current)

(defun mozctoroku-minibuf-menu-seek-prev-subitem (current subitem-alist)
  "subitem-alist に含まれるサブアイテムで、current の前のサブアイテムに
メニューを切り替える. このfunctionの値は前の current の値になる"
  (let (subitem)
    (setq current (1- current))
    (if (< current 0)
	(setq current (1- (car subitem-alist))))
    (setq subitem (cdr (assoc current (cdr subitem-alist))))
    (mozctoroku-minibuf-menu-disp-subitem subitem current (car subitem-alist)))
  current)

(defun mozctoroku-minibuf-menu-seek-head-item ()
  "現在表示しているアイテムの先頭にカーソルを移動"
  (interactive)
  (if (>= emacs-major-version 21)
      (goto-char (minibuffer-prompt-end))
    (goto-char (point-min))))

(defun mozctoroku-minibuf-menu-seek-tail-item ()
  "現在表示しているアイテムの末尾にカーソルを移動"
  (interactive)
  (goto-char (point-max))
  (re-search-backward "\\([0-9]\\): [^ \t]+")
  (goto-char (match-beginning 1)))

(defun mozctoroku-minibuf-menu-seek-next-item ()
  "現在選択しているアイテムの次のアイテムにカーソルを合わせる"
  (interactive)
  ;; 今、いる位置からマッチ指定しているかをチェック (これはできるはず)
  (if (looking-at "\\([0-9]: [^ \t]+\\)")
      (goto-char (match-end 1))) ;; マッチ指定れば、それを無視する
  ;; 次の要素を検索して移動
  (if (not (re-search-forward "\\([0-9]\\): [^ \t]+" (point-max) t))
      (mozctoroku-minibuf-menu-seek-head-item)
    (goto-char (match-beginning 1))))

(defun mozctoroku-minibuf-menu-seek-prev-item ()
  "現在選択しているアイテムの前のアイテムにカーソルを合わせる"
  (interactive)
  (if (equal (point) (point-min))
      (mozctoroku-minibuf-menu-seek-tail-item)
    (if (not (re-search-backward "\\([0-9]\\): [^ \t]+" (point-min) t))
	(mozctoroku-minibuf-menu-seek-tail-item)
      (goto-char (match-beginning 1)))))

(defun mozctoroku-minibuf-menu-seek-n-item (n max)
  "カーソルの位置を N 番目のアイテムに合わせる。MAX にはsubitemの数が入る"
  (if (not (>= n max))
      (progn
	(mozctoroku-minibuf-menu-seek-head-item) ;; 先頭に移動
	(while (> n 0)
	  (setq n (1- n))
	  (mozctoroku-minibuf-menu-seek-next-item)))))

(defun mozctoroku-minibuf-menu-select-item ()
  "現在のカーソルの位置のアイテムの文字列を選択されたアイテムとして
mini-buffer の処理を終了する."
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
  "mini-buffer にメニューを表示する"
  (let (mozctoroku-minibuf-menu-map
	subitem-curent subitem-alist
	initial-label item)
    (setq prompt (format "%s: " prompt))
    (setq menus-string nil)
    
    ;; メニュー用のkeymapを作成
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
    
