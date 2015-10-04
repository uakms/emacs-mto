# emacs-mto

## これは何ですか？
日本語で書かれた文章を昔風の文章に変換することができます。

## どのように使いますか？

1. このリポジトリを ~/elisp/ 以下にクローンします。
2. `init.el` に下記を記述します。

    ```lisp
    ; 毎回 `M-x mto-mode` するのが面倒なら何かのメジャーモードに便乗する
    ;(add-hook 'hoge-mode-hook 'mto-mode)

    ; 同包してある辞書を使う
    (setq mto-dict-dir "~/elisp/emacs-mto/dict")

    ; 他のモードのキーマップと衝突した場合はここで `"\C-c."` とか設定する
    ;(setq mto-prefix-key "\C-c")

    ; これを最後にしとかんと、上の `setq` が効かんのはなんでやねん
    (require 'mto)
    ```

3. 適当なテキストを開いて `M-x mto-mode` とします。
4. 変換したい文章を選択して `C-c t` とすれば仮名使いが昔風になります。

その他のキーバインドについては後日書かれることになるでしょう。

機能としては[このページ](http://nakinor.github.io/mto)でやっていることと同じものです。
