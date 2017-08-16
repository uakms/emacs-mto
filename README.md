# emacs-mto

## これは何ですか？
日本語で書かれた文章を昔風の文章に変換することができます。

## どのように使いますか？

1. このリポジトリを ~/elisp/ 以下にクローンします。
2. [ここ](https://github.com/uakms/mto)から辞書をクローンもしくはダウンロードします。
3. `init.el` に下記を記述します。`mto-dict-dir` の場所は 2. でダウンロードした辞書へのパスに変更してください。

    ```lisp
    ; 毎回 `M-x mto-mode` するのが面倒なら何かのメジャーモードに便乗する
    ;(add-hook 'hoge-mode-hook 'mto-mode)

    ; GitHubからダウンロードした辞書を使う
    (setq mto-dict-dir "/Users/your/downloaded/dictionary")

    ; 他のモードのキーマップと衝突した場合はここで `"\C-c."` とか設定する
    ;(setq mto-prefix-key "\C-c")

    ; 置換されたキーワードに色が付くのがウザすぎる場合は `nil` にする
    ;(setq mto-colorize-word t)

    ; GUI 版の Emacs を使っている時に表示されるメニューバーの言語
    ; 'ja 以外の場合はなんちゃって英語になる
    ;(setq mto-menu-lang 'ja)

    ; これを最後にしとかんと、上の `setq` 達が効かんのはなんでやねん
    (require 'mto)
    ```

4. 適当なテキストを開いて `M-x mto-mode` とします。
5. 変換したい文章を選択して `C-c t` とすれば仮名使いが昔風になります。

その他のキーバインドについては後日書かれることになるでしょう。
後日になりました。

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

機能としては[このページ](http://uakms.github.io/mto)でやっていることと同じものです。
