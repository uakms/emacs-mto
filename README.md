# README

## これは何ですか？
日本語で書かれた文章を昔風の文章に変換します。

## どのように使いますか？
リポジトリを ~/elisp/ 以下にクローンして、`init.el` に

    (setq mto-dict-dir "~/elisp/emacs-mto/dict")
    (require 'mto)

を記述します。適当なテキストを開いて `C-c t` とすれば仮名遣いが古くなります。その他のキーバインドについては後日書かれることになるでしょう。

機能としては[このページ](http://nakinor.github.io/mto)でやっていることと同じものです。

