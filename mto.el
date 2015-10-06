;;; mto.el --- mto (Mojiretsu wo Tanjun ni Okikae masu.)

;; Author: nakinor
;; Created: 2011-05-12
;; Revised: 2015-10-06

;;; Commentary:

;; See README

;;; Code:
;; 初期設定
;; init.el に書いても良い。その場合は (require 'mto) を最後に記述すること
;; そうしないとプレフィクスキーの変更が反映されないのだ！
;;
;(setq mto-dict-dir "~/elisp/emacs-mto/dict")  ; 辞書の場所
;(setq mto-prefix-key "\C-c")                  ; プレフィクスキー
;(setq mto-colorize-word t)                    ; キーワードの色付け
;(setq mto-menu-lang 'ja)                      ; メニューバーの言語

;; ライブラリの読み込み
(require 'mto-mode)
(require 'mto-vars)
(require 'mto-menu)
(require 'mto-color)
(require 'mto-jisyo-edit)
(with-eval-after-load 'twittering-mode
  (require 'mto-tweet))

(provide 'mto)

;;; mto.el ends here
