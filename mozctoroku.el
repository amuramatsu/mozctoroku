;; -*- Emacs-Lisp -*-
;; mozctoroku.el          mozc ユーザ辞書登録を行う

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; This program based on:
;; wxgtoroku.el           wxg ユーザ辞書登録を行う
;;
;; Author: Takeshi KATO <kato@mickey.ai.kyutech.ac.jp>
;; Maintainer: Takeshi KATO <kato@mickey.ai.kyutech.ac.jp>
;; Created: January 2001
;; Version: 0.02.13 beta 5

;;
;;
;;◎ 使い方
;;
;;  [ .emacs での設定 ]
;; 
;;        (autoload 'mozctoroku-toroku "mozctoroku"
;;                  "MOZC ユーザ辞書登録" t nil)
;;
;;  [ 操作方法 ]
;;
;;     使い方は多分ノリでわかるとは思いますが一応書いておきます。
;;     まず、M-x mozctoroku-toroku と実行します。
;;
;;     次に、辞書に登録する単語を聞いてきますので、入力してください。
;;     そのあとに単語の読みを聞いてきますので、これも入力します。
;;     この２つは通常のミニバッファで文字列を入力するのと同じキーが
;;     使えるはずです。
;;
;;     最後に辞書に登録する際の品詞を聞いてきますので、選択してください。
;;     品詞の選択には以下のキーが使えます。
;;
;;         C-g     キャンセル
;;         Enter   項目を選択
;;         C-a     左端の項目へ移動
;;         C-e     右端の項目へ移動
;;         C-b     一つ左の項目に移動
;;         C-f     一つ右の項目に移動
;;         C-p     前のメニューに移動
;;         C-n     次のメニューに移動
;;        
;;     最後の２つは意味がわかりにくいとは思うのですが、メニューの
;;     表示は４つずつになってます。これはできる限り一度に表示できる
;;     ようにしたかったからなんです。それで項目数が５つをこえる場合に
;;     項目を切り替えるのに最後の２つを利用します。
;;
;;     あと、数字キーを押すことで、アイテムを選択(実際にはEnterを
;;     押す必要があります)することができます。
;;

(require 'mozctoroku-minibuf-menu)

;; バージョン
(defconst mozctoroku-version "0.0.1 alpha")
(defun mozctoroku-version ()
  (interactive)
  (message "mozctoroku version %s" mozctoroku-version))

;; mozc-dict のコマンドの場所を指定
(defvar mozctoroku-mozc-dict-command nil
  "mozc-dict のコマンド名を指定する。nil の場合は mozc-dict を使う。
例:
\(setq mozctoroku-mozc-dict-command \"/usr/bin/mozc-dict\")")

(defvar mozctoroku-logfile nil
  "*辞書登録の際に、登録した単語を記録するログファイルを指定する。
nil の場合にはログは記録されない")
(defvar mozctoroku-word-at-point nil
  "*辞書登録を行なう際にカーソルの位置の単語を取り出すか？を決める。
フラグ。non-nil ならば、word-at-pointを使い、カーソル位置の単語を取り出す")

(defun mozctoroku-without-newline (beg end)
  "リージョン内に改行コードを含むか判定する"
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

;; XEmacs 以外では region-exists-p が使えないので、
;; region-exists-p を定義
(defun mozctoroku-region-exists-p ()
  (if (featurep 'xemacs)
      (region-exists-p)
    mark-active))

;; 品詞名選択用のアイテムリスト
(defconst mozctoroku-hinsi-list
  '(("名詞" (("普通名詞" ("名詞"
			  "名詞形動"
			  "名詞サ変"))
	     ("固有名詞" (("姓名" ("姓" "名" "人名"))
			  "地名"
			  "組織名"
			  "固有名詞"))
	     "短縮よみ"))
    ("動詞" (("動詞五段" ("ワ行五段"
			  "カ行五段"
			  "サ行五段"
			  "タ行五段"
			  "ナ行五段"
			  "マ行五段"
			  "ラ行五段"
			  "ガ行五段"
			  "バ行五段"))
	     "動詞一段"
	     ("変格活用" ("動詞カ変"
			  "動詞サ変"
			  "動詞ザ変")
	     ("特殊活用" ("動詞ラ変"
			  "ハ行四段")))))
    ("形容詞" ("形容詞" "形容動詞"))
    ("修飾語" ("副詞" "連体詞"))
    ("独立語" ("記号" "顔文字" "感動詞" "独立語" "単漢字" "数" "Alphabet"))
    ("接頭尾語" ("接頭語" "助数詞" "接尾一般" "接尾人名" "接尾地名"))
    ("その他" ("接続詞" "終助詞" "句読点" "抑制単語")))
  "品詞選択のメニュー項目のリスト定義")

(defconst mozctoroku-mozc-hinsi-alist
  '(("Alphabet" . "アルファベット")
    ("形容動詞" . "名詞形動")
    ("単漢字" . "名詞")
    ("ワ行五段" . "動詞ワ行五段")
    ("カ行五段" . "動詞カ行五段")
    ("サ行五段" . "動詞サ行五段")
    ("タ行五段" . "動詞タ行五段")
    ("ナ行五段" . "動詞ナ行五段")
    ("マ行五段" . "動詞マ行五段")
    ("ラ行五段" . "動詞ラ行五段")
    ("ガ行五段" . "動詞ガ行五段")
    ("バ行五段" . "動詞バ行五段")
    ("ハ行四段" . "動詞ハ行四段")))

(defvar mozctoroku-back-menu-string "戻る")

(defun mozctoroku-create-menu-list (menu-list &optional submenu)
  "mozctoroku-hinsi-list のようなリストからメニューをつくり出す"
  (let (item (created-menu nil))
    (while menu-list
      (setq item (car menu-list))
      (setq menu-list (cdr menu-list))
      ;; サブメニューを持っている場合はリストになるので、
      ;; その場合は先頭要素を item とする
      (if (listp item)
	  (setq item (car item)))
      
      (if created-menu
	  (setq created-menu (append created-menu (cons item nil)))
	(setq created-menu (cons item nil))))

    (if submenu
	(cons mozctoroku-back-menu-string created-menu)
      created-menu)))

(defun mozctoroku-hinsi-menu (label hlist submenu-flag)
  "引数 HLIST から品詞を取り出す"
  (let ((current-menu (mozctoroku-create-menu-list hlist submenu-flag))
	(loop-flag t)
	selected item hinsi)
    (while loop-flag
      (setq loop-flag nil)
      (setq selected (mozctoroku-minibuf-menu label current-menu))
      (setq item (assoc selected hlist))
      (if (equal selected mozctoroku-back-menu-string)
	  ;; 戻るが選択された
	  (setq hinsi nil)
	(if item
	    (progn ;; サブメニューが存在する
	      (setq hinsi
		    (mozctoroku-hinsi-menu selected (car (cdr item)) t))
	      (if (not hinsi) ;; サブメニューで戻るが選択された
		  (setq loop-flag t)))
	  ;; サブメニューが存在しない
	  (setq hinsi selected))))
    hinsi))

(defun mozctoroku-userdic-menu (label dlist)
  "引数 DLIST から辞書を取り出す"
  (let (current-menu userdic)
    (setq current-menu (mozctoroku-create-menu-list dlist nil))
    (setq userdic (mozctoroku-minibuf-menu label current-menu))
    userdic))

(defvar mozctoroku-setup-read-japanese-string-hook nil)
(defvar mozctoroku-exit-read-japanese-string-hook nil)

(defun mozctoroku-read-japanese-string
  (prompt &optional initial history default mehtod)
  "minibuffer で日本語の文字列を入力させる。
INITIAL はユーザが入力する前から入力されているデフォルト文字列。
HISOTRY は入力履歴として使う変数を指定する。"
  (let (input
	(enter-hook minibuffer-setup-hook)
	(exit-hook minibuffer-exit-hook))
    (unwind-protect
	(progn
	  ;; ミニバッファで日本語入力をデフォルトに
	  (add-hook 'minibuffer-setup-hook
		    '(lambda ()
		       (activate-input-method default-input-method)))
	  (add-hook 'minibuffer-exit-hook
		    '(lambda ()
		       (inactivate-input-method)))
	  ;; hook の実行
	  (run-hooks 'mozctoroku-setup-read-japanese-string-hook)
	  (setq input
		(read-from-minibuffer prompt initial nil nil history)))
      ;; 後始末
      (run-hooks 'mozctoroku-exit-read-japanese-string-hook)
      (setq minibuffer-setup-hook enter-hook)
      (setq minibuffer-exit-hook exit-hook))
    input))

(defun mozctoroku-toroku ()
  "単語をmozcのユーザ辞書に登録する"
  (interactive)
  (cond ((mozctoroku-region-exists-p) ;; リージョンが指定されている
	 (let ((start (mark))
	       (end (point)) tmp)
	   ;; start < end となるように修正
	   (if (> start end)
	       (mozctoroku-toroku-region end start)
	   (mozctoroku-toroku-region start end))))
	(mozctoroku-word-at-point     ;; word-at-point による単語切り出し
	 (mozctoroku-word-at-point))
	(t                            ;; それ以外
	 (mozctoroku-session ""))))

(defun mozctoroku-word-at-point ()
  "カーソルの位置の単語を登録する"
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
  "領域をmozcのユーザ辞書に登録する"
  (interactive "r")
  ;; start < end になるように
  (if (> start end)
      (progn
	(setq end (- start end))
	(setq start (- start end))
	(setq end (+ end start))))
  ;; 登録処理
  (if (mozctoroku-without-newline start end)
      (let ((toroku-word (buffer-substring-no-properties start end)))
	(mozctoroku-session toroku-word))
    (mozctoroku-session "")))

;; mozctoroku-session からのみ呼び出される関数群
(defun mozctoroku-read-tango (toroku-word)
  "登録する単語を読み込み、それを戻す関数。
TOROKU-WORD はミニバッファで登録する単語を入力する際のデフォルト
文字列となる。"
  (let ((word ""))
    (while (equal word "")
      (setq word
	    (mozctoroku-read-japanese-string
	     "単語? "
	     toroku-word 'mozctoroku-tango-str-history))
      (when (equal word "")
	(message "登録単語を入力してください.")
	(sit-for 1)))
    word))
(defun mozctoroku-read-yomi (toroku-word &optional yomi)
  "登録する単語の読みを読み込み、それを戻す関数。"
  (let ((word ""))
    (while (equal word "")
      (setq word
	    (mozctoroku-read-japanese-string
	     (format "単語[%s] 読み? " toroku-word)
	      yomi 'mozctoroku-yomi-str-history))
      (when (equal word "")
	(message "読みを入力してください.")
	(sit-for 1)))
    word))
;; end

(defvar mozctoroku-tango-str-history nil
  "登録時の単語の履歴を格納する変数。")
(defvar mozctoroku-yomi-str-history nil
  "登録時の読みの履歴を格納する変数。")

;; mozctoroku-session から利用する read-japanse-string-hook
(defun mozctoroku-session-setup-read-string-hook ()
  "minibuffer 中の仮名漢字変換の toggle で modeline の表示を変えないように"
  (setq display-minibuffer-mode-in-minibuffer t))
(defun mozctoroku-session-exit-read-string-hook ()
  "setup-hook で行なった設定を元に戻す"
  (setq display-minibuffer-mode-in-minibuffer nil))

(defun mozctoroku-session (toroku-word)
  "mozctoroku-toroku, mozctoroku-toroku-region に共通の登録処理"
  (interactive)
  (let ((old-setup-hook mozctoroku-setup-read-japanese-string-hook)
	(old-exit-hook mozctoroku-exit-read-japanese-string-hook)
	(old-minibuf-setup-hook mozctoroku-minibuf-menu-setup-hook)
	(old-minibuf-exit-hook mozctoroku-minibuf-menu-exit-hook)
	word yomi hinsi userdic)
    ;; 単語登録に必要な情報の登録
    (unwind-protect
	(progn
	  ;; フックを用いて登録中の mode-line の表示を固定
	  (add-hook 'mozctoroku-setup-read-japanese-string-hook
		    'mozctoroku-session-setup-read-string-hook)
	  (add-hook 'mozctoroku-exit-read-japanese-string-hook
		    'mozctoroku-session-exit-read-string-hook)
	  (add-hook 'mozctoroku-minibuf-menu-setup-hook
		    'mozctoroku-session-setup-read-string-hook)
	  (add-hook 'mozctoroku-minibuf-menu-exit-hook
		    'mozctoroku-session-exit-read-string-hook)
	  
	  ;; 実際の単語の読み込み
	  (setq word (mozctoroku-read-tango toroku-word))
	  (setq yomi (mozctoroku-read-yomi word))
	  
	  (setq hinsi
		(mozctoroku-hinsi-menu "品詞" mozctoroku-hinsi-list nil)))
      ;; フックの設定を戻す
      (setq mozctoroku-setup-read-japanese-string-hook old-setup-hook)
      (setq mozctoroku-exit-read-japanese-string-hook old-exit-hook)
      (setq mozctoroku-minibuf-menu-setup-hook old-minibuf-setup-hook)
      (setq mozctoroku-minibuf-menu-exit-hook old-minibuf-exit-hook))
    
    ;; 辞書への登録
    (mozctoroku-process word yomi hinsi)))

(defun mozctoroku-log (word yomi hinsi)
  "ログファイルに登録する単語を記録する"
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
      ;; 以前のログの内容をコピー
      (if (file-exists-p mozctoroku-log-file)
	  (insert-file mozctoroku-log-file))
      ;; 最後にログを追加
      (goto-char (point-max))
      (insert (format "%s\t\"%s\":%s\n" yomi word hinsi))
      (write-file mozctoroku-logfile)
      (kill-buffer buf))))

(defun mozctoroku-process (word yomi hinsi)
  "実際の登録作業を行なう"
  (let ((mozc-dict (or mozctoroku-mozc-dict-command "mozc-dict")))
    ;; ログファイルへ記録
    (if mozctoroku-logfile
	(mozctoroku-log word yomi hinsi))
    (let ((coding-system-for-read 'utf-8)
	  (coding-system-for-write 'utf-8)
	  (xhinsi (assoc hinsi mozctoroku-mozc-hinsi-alist)))
      (setq xhinsi (if xhinsi (cdr xhinsi) hinsi))
      (call-process mozc-dict nil nil nil "-s" yomi word xhinsi))
  (message (format "%s(%s) を登録しました。" word yomi))))

(provide 'mozctoroku)
;; end of mozctoroku
