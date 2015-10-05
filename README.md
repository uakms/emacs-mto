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

    ; 置換されたキーワードに色が付くのがウザすぎる場合は `nil` にする
    ;(setq mto-colorize-word t)

    ; これを最後にしとかんと、上の `setq` 達が効かんのはなんでやねん
    (require 'mto)
    ```

3. 適当なテキストを開いて `M-x mto-mode` とします。
4. 変換したい文章を選択して `C-c t` とすれば仮名使いが昔風になります。

その他のキーバインドについては後日書かれることになるでしょう。

|目的|キーマップ|備考|
|:---|:---:|:---|
|現代かな使いを旧かな使いへ| `C-c t` | `M-x mto-region-trad-orth` |
|漢字の新字体を旧字体へ    | `C-c o` | `M-x mto-region-old-char` |
|旧かな使いを現代かな使いへ| `C-c m` | `M-x mto-region-modern-orth` |
|漢字の旧字体を新字体へ    | `C-c n` | `M-x mto-region-new-char` |
|関西弁にする              | `C-c k` | `M-x mto-region-kansai` |
|ハングル文字をひらがなへ  | `C-c h` | `M-x mto-region-hangeul` |
|よく間違える旧かなチェック| `C-c c` | `M-x mto-region-check-traditional`|
|色付けを解除する          | `C-c u` | `M-x mto-clear-color` |

機能としては[このページ](http://nakinor.github.io/mto)でやっていることと同じものです。
