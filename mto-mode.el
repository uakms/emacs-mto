;;; mto-mode.el --- mto minor mode.

;; Author: nakinor
;; Created: 2015-10-06
;; Revised: 2015-10-06

;;; Commentary:

;; See README

;;; Code:

;; キーマップ
(defvar mto-prefix-key "\C-c" "mto prefix key setting.")
(defvar mto-default-keymap
  '(("t" mto-region-trad-orth)
    ("m" mto-region-modern-orth)
    ("o" mto-region-old-char)
    ("n" mto-region-new-char)
    ("k" mto-region-kansai)
    ("h" mto-region-hangeul)
    ("c" mto-region-check-traditional)
    ("u" mto-clear-color)
;    ("]p" mto-region-ruby-plain)
;    ("]h" mto-region-ruby-html)
;    ("]l" mto-region-ruby-latex)
    ) "mto default keymap")

;; マイナーモード設定
(easy-mmode-define-minor-mode mto-mode
  "This is MTO Mode." ; 説明文
  nil                 ; 初期値は有効
  " MTO"              ; モードラインに表示する文字
  ; マイナーモードでのキーバインド設定
  ; バッファ全体を対象とする置換はメニューバーから選択か M-x で
  (mapcar (lambda (elem)
            (let ((key (first elem))
                  (func (second elem)))
              (cons (concat mto-prefix-key key) func)))
          mto-default-keymap))

; 置換された単語に色付けをするか否か
(defvar mto-colorize-word t) ;初期値は有効

;; 辞書ファイルの場所を設定
(defvar mto-dict-dir "~/elisp/emacs-mto/dict")
(defvar mto-kanajisyo (concat mto-dict-dir "/kana-jisyo"))
(defvar mto-kanjijisyo (concat mto-dict-dir "/kanji-jisyo"))
(defvar mto-checkjisyo (concat mto-dict-dir "/check-jisyo"))
(defvar mto-rubyjisyo (concat mto-dict-dir "/ruby-jisyo"))
(defvar mto-kansaijisyo (concat mto-dict-dir "/kansai-jisyo"))
(defvar mto-hangeuljisyo (concat mto-dict-dir "/hangeul-jisyo"))

;; 挿入するタグの種類を設定
(defvar mto-plain-tag '("" "(" ")"))
(defvar mto-html-tag '("<ruby>" "<rp>(</rp><rt>" "</rt><rp>)</rp></ruby>"))
(defvar mto-latex-tag '("\\\\ruby{" "}{" "}"))

;; 辞書ファイルからハッシュを作成する際に利用する部品
(defun mto-replace (findword replaceword)
  "バッファの先頭に移動してから置換をし、辞書から連想リストへ整形するための部品"
  (goto-char (point-min))
  (while (re-search-forward findword nil t)
    (replace-match replaceword)))

;; タイマー
(defun mto-timer (start-t stop-t)
  "処理にかかった時間を計算するための部品"
  (- (+ (* 65536 (car stop-t))
        (car (cdr stop-t))
        (/ (car (cdr (cdr stop-t))) 1000000.0))
     (+ (* 65536 (car start-t))
        (car (cdr start-t))
        (/ (car (cdr (cdr start-t))) 1000000.0))))

;; Parser (連想リストを作る)
; ハッシュに代入するために辞書を整形する (mto-alist が出来る)
; こんな感じ -> (("笑う" . "笑ふ") ("疑う" . "疑ふ") ... )
(defun mto-parser (jisyo-name)
  "辞書ファイルから連想リストを作成する。引数には自作の辞書ファイルを指定する"
  (with-temp-buffer
    (insert-file-contents jisyo-name) ; 辞書を tmp バッファに読み込む
    (mto-replace ";.*" "")            ; コメント行を削除(改行のみ残っている)
    (mto-replace " +$" "")            ; 末端の空白を削除
    (mto-replace "^\n" "")            ; 改行のみの行を削除
    (mto-replace " /" "\" . \"")      ;「 /」を「" . "」に置換
    (mto-replace "\n" "\") (\"")      ; 改行を「") ("」に置換
    (mto-replace " (\"+$" "")         ; 最後の「 ("」を削除
    (goto-char (point-min))           ; tmp バッファの先頭に移動
    (insert "(setq mto-alist '((\"")  ; 先頭に「(setq mto-alist '(("」を挿入
    (goto-char (point-max))           ; tmp バッファの末尾に移動
    (insert "))")                     ; 最後に「))」を挿入
    (eval-buffer)))                   ; 評価して mto-alist に代入

;; 置換の本体 (car -> cdr)
(defun mto-search-replace-main-cdr ()
  "置換するための連想リスト(mto-alist)を元にして検索置換を実行する (car->cdr)"
  (save-excursion
    (mapcar (lambda (x)
              (goto-char (point-min))
              (while (re-search-forward (car x) nil t)
                (replace-match (cdr x))))
            mto-alist)))

;; 置換の本体 (cdr -> car)
(defun mto-search-replace-main-car ()
  "置換するための連想リスト(mto-alist)を元にして検索置換を実行する (cdr->car)"
  (save-excursion
    (mapcar (lambda (x)
              (goto-char (point-min))
              (while (re-search-forward (cdr x) nil t)
                (replace-match (car x))))
            mto-alist)))

;; 置換の本体 (car -> cdr) Plain, HTML, LaTeX 用
(defun mto-search-replace-main-ruby (atag)
  "置き換えるタグの配列を受け取って連想リスト(mto-alist)を元にして
   検索置換を実行する (car -> cdr)"
  (save-excursion
    (mapcar (lambda (x)
              (goto-char (point-min))
              (while (re-search-forward (car x) nil t)
                (replace-match
                 (concat (car atag) (car x) (cadr atag) (cdr x) (caddr atag))
                 )))
            mto-alist)))

;; 各種の置換手続の設定(バッファ全体が対象)
(defun mto-trad-orth ()
  "現代仮名使いから歴史的仮名使いの文章へ変換する"
  (interactive)
  (setq mto-start-t (current-time))      ; タイマースタート
  (message "歴史的仮名使いの文章に変換しています...")
  (if (equal mto-colorize-word t)        ; 色付けは置換をする前に指定
      (progn
        (mto-clear-color)                ; 以前の漢字と仮名への色付けを解除
        (create-color-keywords-cdr mto-kanajisyo "kana-trad-face")))
  (mto-parser mto-kanajisyo)             ; 辞書からリストを作成する
  (mto-search-replace-main-cdr)          ; 置換作業
  (setq mto-stop-t (current-time))       ; タイマーストップ
  (message "%0.2f sec かかったけど、とりあえず終ったみたい(^^;)"
           (mto-timer mto-start-t mto-stop-t)))

(defun mto-modern-orth ()
  "歴史的仮名使いから現代仮名使いの文章へ変換する"
  (interactive)
  (setq mto-start-t (current-time))
  (message "現代仮名使いの文章に変換しています...")
  (if (equal mto-colorize-word t)
      (progn
        (mto-clear-color)
        (create-color-keywords-car mto-kanajisyo "kana-modern-face")))
  (mto-parser mto-kanajisyo)
  (mto-search-replace-main-car)
  (setq mto-stop-t (current-time))
  (message "%0.2f sec かかったけど、とりあえず終ったみたい(^^;)"
           (mto-timer mto-start-t mto-stop-t)))

(defun mto-old-char ()
  "新字体を旧字体へ変換する"
  (interactive)
  (setq mto-start-t (current-time))
  (message "新字体を旧字体に変換しています...")
  (if (equal mto-colorize-word t)
      (progn
        (mto-clear-color)
        (create-color-keywords-cdr mto-kanjijisyo "kanji-trad-face")))
  (mto-parser mto-kanjijisyo)
  (mto-search-replace-main-cdr)
  (setq mto-stop-t (current-time))
  (message "%0.2f sec かかったけど、とりあえず終ったみたい(^^;)"
           (mto-timer mto-start-t mto-stop-t)))

(defun mto-new-char ()
  "旧字体を新字体へ変換する"
  (interactive)
  (setq mto-start-t (current-time))
  (message "旧字体を新字体に変換しています...")
  (if (equal mto-colorize-word t)
      (progn
        (mto-clear-color)
        (create-color-keywords-car mto-kanjijisyo "kanji-modern-face")))
  (mto-parser mto-kanjijisyo)
  (mto-search-replace-main-car)
  (setq mto-stop-t (current-time))
  (message "%0.2f sec かかったけど、とりあえず終ったみたい(^^;)"
           (mto-timer mto-start-t mto-stop-t)))

(defun mto-kansai ()
  "関西弁へ変換する"
  (interactive)
  (setq mto-start-t (current-time))
  (message "関西弁に変換しています...")
  (if (equal mto-colorize-word t)
      (progn
        (mto-clear-color)
        (create-color-keywords-cdr mto-kansaijisyo "mto-ruby-face")))
  (mto-parser mto-kansaijisyo)
  (mto-search-replace-main-cdr)
  (setq mto-stop-t (current-time))
  (message "%0.2f sec かかったけど、とりあえず終ったみたい(^^;)"
           (mto-timer mto-start-t mto-stop-t)))

(defun mto-hangeul ()
  "ハングルからひらがなへ変換する"
  (interactive)
  (setq mto-start-t (current-time))
  (message "ハングルをひらがなに変換しています...")
;  (if (equal mto-colorize-word t)
;      (progn
;        (mto-clear-color)
;        (create-color-keywords-car mto-hangeuljisyo "mto-ruby-face")))
  (mto-parser mto-hangeuljisyo)
  (mto-search-replace-main-car)
  (setq mto-stop-t (current-time))
  (message "%0.2f sec かかったけど、とりあえず終ったみたい(^^;)"
           (mto-timer mto-start-t mto-stop-t)))

(defun mto-check-traditional ()
  "歴史的仮名使いの誤りを色付けする(変換はしない)"
  (interactive)
  (setq mto-start-t (current-time))
  (message "誤りやすい仮名使いをチェックしています...")
  (if (equal mto-colorize-word t)
      (progn
        (mto-clear-color)
        (create-color-keywords-car mto-checkjisyo "mto-check-face")))
  (mto-parser mto-checkjisyo)
  (mto-search-replace-main-cdr)
  (setq mto-stop-t (current-time))
  (message "%0.2f sec かかったけど、とりあえず終ったみたい(^^;)"
           (mto-timer mto-start-t mto-stop-t)))

(defun mto-ruby-plain ()
  "読みを括弧付けで挿入する"
  (interactive)
  (if (equal mto-colorize-word t)
      (progn
        (mto-clear-color)
        (create-color-keywords-car mto-rubyjisyo "mto-ruby-face")))
  (mto-parser mto-rubyjisyo)
  (mto-search-replace-main-ruby mto-plain-tag))

(defun mto-ruby-html ()
  "読みを html タグ付けで挿入する"
  (interactive)
  (if (equal mto-colorize-word t)
      (progn
        (mto-clear-color)
        (create-color-keywords-car mto-rubyjisyo "mto-ruby-face")))
  (mto-parser mto-rubyjisyo)
  (mto-search-replace-main-ruby mto-html-tag))

(defun mto-ruby-latex ()
  "読みを LaTeX タグ付けで挿入する"
  (interactive)
  (if (equal mto-colorize-word t)
      (progn
        (mto-clear-color)
        (create-color-keywords-car mto-rubyjisyo "mto-ruby-face")))
  (mto-parser mto-rubyjisyo)
  (mto-search-replace-main-ruby mto-latex-tag))

;; 各種の置換手続の設定(リージョン範囲が対象)
(defun mto-region-trad-orth (start end)
  "リージョンの範囲内について現代仮名使いから歴史的仮名使いの文章へ変換"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mto-trad-orth))))

(defun mto-region-modern-orth (start end)
  "リージョンの範囲内について歴史的仮名使いから現代仮名使いの文章へ変換"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mto-modern-orth))))

(defun mto-region-old-char (start end)
  "リージョンの範囲内について新字体を旧字体へ変換"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mto-old-char))))

(defun mto-region-new-char (start end)
  "リージョンの範囲内について旧字体を新字体へ変換"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mto-new-char))))

(defun mto-region-kansai (start end)
  "リージョンの範囲内について関西弁へ変換"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mto-kansai))))

(defun mto-region-hangeul (start end)
  "リージョンの範囲内についてハングルをひらがなへ変換"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mto-hangeul))))

(defun mto-region-check-traditional (start end)
  "リージョンの範囲内について歴史的仮名使いの誤りを色付けする(変換はしない)"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mto-check-traditional))))

(defun mto-region-ruby-plain (start end)
  "リージョンの範囲内について読みを括弧付けで挿入する"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mto-ruby-plain))))

(defun mto-region-ruby-html (start end)
  "リージョンの範囲内について読みを html タグ付けで挿入する"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mto-ruby-html))))

(defun mto-region-ruby-latex (start end)
  "リージョンの範囲内について読みを LaTeX タグ付けで挿入する"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mto-ruby-latex))))

(provide 'mto-mode)

;;; mto-mode.el ends here
