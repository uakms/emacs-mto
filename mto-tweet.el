;;; mto-tweet.el --- tweet replaced string

;; Author: nakinor
;; Created: 2013-02-20
;; Revised: 2013-02-22

;;; Commentary:

;;; Code:
;; ライブラリの読み込み
(require 'twittering-mode)

(defun mto-tweet-message (text)
  "引数のテキストを twittering-mode を利用して投稿する手続"
  (when (and (string= "Japanese" current-language-environment)
             (or (< 21 emacs-major-version)
                 (eq 'utf-8 (terminal-coding-system))))
    (if (< (length text) 141) ; 140文字以下なら投稿
        (progn
          (twittering-call-api 'update-status `((status . ,text)))
          (message "投稿しました"))
      (message "140文字を越えているので投稿できませんでした"))))

(defun mto-tweet-replaced-string (opt)
  "変換してツイートする基本手続"
  (let ((current-string
         (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

; if 文の else then 以下は順次実行されるので progn で囲まなくても良い
; 引数のオプションは下記とする
;   kn: 仮名遣いのみ変換して投稿
;   kj: 漢字のみ変換して投稿
;   kk: 仮名使いと漢字の両方を変換して投稿
;   nr: 何もしないで投稿

; ここで実行されたいのは kn, kk
    (if (or (string-match opt "kn") (string-match opt "kk"))
        (progn
          (mto-parser mto-kanajisyo)
          (mapcar (lambda (x)
                    (setq current-string
                          (replace-regexp-in-string
                           (car x) (cdr x) current-string)))
                  mto-alist)))

; ここで実行されたいのは kj, kk
    (if (or (string-match opt "kj") (string-match opt "kk"))
        (progn
          (mto-parser mto-kanjijisyo)
          (mapcar (lambda (x)
                    (setq current-string
                          (replace-regexp-in-string
                           (car x) (cdr x) current-string)))
                  mto-alist)))

; ここで実行されたいのは kn, kj, kk, nr
    (mto-tweet-message current-string)
    ))

(defun mto-tweet-seikana ()
  "現代仮名使いを歴史的仮名使いに変換してツイートする"
  (interactive)
  (mto-tweet-replaced-string "kn"))

(defun mto-tweet-seiji ()
  "漢字を旧字体に変換してツイートする"
  (interactive)
  (mto-tweet-replaced-string "kj"))

(defun mto-tweet-seijiseikana ()
  "旧字旧仮名に変換してツイートする"
  (interactive)
  (mto-tweet-replaced-string "kk"))

(defun mto-tweet-current-text ()
  "カーソルのある現在行のテキストをツイートする"
  (interactive)
  (mto-tweet-replaced-string "nr"))

(provide 'mto-tweet)

;;; mto-tweet.el ends here
