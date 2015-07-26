;;; mto-menu.el --- add mto commands to Emacs menu bar

;; Author: nakinor
;; Created: 2012-06-23
;; Revised: 2015-04-10

;;; Commentary:

;;; Code:
(require 'easymenu)
(defvar mto-menu-lang 'ja)

(defvar mto-mode-menu-spec-en
  '("Mto"
    ["Replace from Modern kana to Traditional kana"
     mto-trad-orth t]
    ["Replace from Modern kana to Traditional kana (in region)"
     mto-region-trad-orth mark-active]
    ["Replace from Traditional kana to Modern kana"
     mto-modern-orth t]
    ["Replace from Traditional kana to Modern kana (in region)"
     mto-region-modern-orth mark-active]
    "----"
    ["Replace from New kanji to Old kanji"
     mto-old-char t]
    ["Replace from New kanji to Old kanji (in region)"
     mto-region-old-char mark-active]
    ["Replace from Old kanji to New kanji"
     mto-new-char t]
    ["Replace from Old kanji to New kanji (in region)"
     mto-region-new-char mark-active]
    "----"
    ["Replace to Kansai-ben"
     mto-kansai t]
    ["Replace to Kansai-ben (in region)"
     mto-region-kansai mark-active]
    "----"
    ["Check Traditional kana Orthography"
     mto-check-traditional t]
    ["Check Traditional kana Orthography (in region)"
     mto-region-check-traditional mark-active]
    "----"
    ["Replace obscure kanji with parenthese"
     mto-ruby-plain t]
    ["Replace obscure kanji with parenthese (in region)"
     mto-region-ruby-plain mark-active]
    ["Replace obscure kanji with html tag"
     mto-ruby-html t]
    ["Replace obscure kanji with html tag (in region)"
     mto-region-ruby-html mark-active]
    ["Replace obscure kanji with latex tag"
     mto-ruby-latex t]
    ["Replace unreadable kanji with latex tag (in region)"
     mto-region-ruby-latex mark-active]
    "----"
    ("Dictionary Tool"
     ["Check kana association list"
      mto-kanajisyo-kakunin t]
     ["Check kanji association list"
      mto-kanjijisyo-kakunin t]
     ["Check obscure kana jisyo"
      mto-checkjisyo-kakunin t]
     ["Check ruby jisyo"
      mto-rubyjisyo-kakunin t]
     ["Check Kansai-ben jisyo"
      mto-kansaijisyo-kakunin t]
     ["Check Hangeul jisyo"
      mto-hangeuljisyo-kakunin t])
    ))

(defvar mto-mode-menu-spec-ja
  '("Mto"
    ["新仮名から旧仮名へ変換"
     mto-trad-orth t]
    ["新仮名から旧仮名へ変換 (選択範囲)"
     mto-region-trad-orth mark-active]
    ["旧仮名から新仮名へ変換"
     mto-modern-orth t]
    ["旧仮名から新仮名へ変換 (選択範囲)"
     mto-region-modern-orth mark-active]
    "----"
    ["新字体から旧字体へ変換"
     mto-old-char t]
    ["新字体から旧字体へ変換 (選択範囲)"
     mto-region-old-char mark-active]
    ["旧字体から新字体へ変換"
     mto-new-char t]
    ["旧字体から新字体へ変換 (選択範囲)"
     mto-region-new-char mark-active]
    "----"
    ["関西弁へ変換"
     mto-kansai t]
    ["関西弁へ変換 (選択範囲)"
     mto-region-kansai mark-active]
    "----"
    ["誤りやすい仮名使いのチェック"
     mto-check-traditional t]
    ["誤りやすい仮名使いのチェック (選択範囲)"
     mto-region-check-traditional mark-active]
    "----"
    ["難読漢字にふりがなを付ける"
     mto-ruby-plain t]
    ["難読漢字にふりがなを付ける (選択範囲)"
     mto-region-ruby-plain mark-active]
    ["難読漢字にHTMLのふりがなを付ける"
     mto-ruby-html t]
    ["難読漢字にHTMLのふりがなを付ける (選択範囲)"
     mto-region-ruby-html mark-active]
    ["難読漢字にLaTeXのふりがなを付ける"
     mto-ruby-latex t]
    ["難読漢字にLaTeXのふりがなを付ける (選択範囲)"
     mto-region-ruby-latex mark-active]
    "----"
    ("辞書ツール"
     ["かな辞書の重複をチェック"
      mto-kanajisyo-kakunin t]
     ["漢字辞書の重複をチェック"
      mto-kanjijisyo-kakunin t]
     ["誤りやすいかな使い辞書の重複をチェック"
      mto-checkjisyo-kakunin t]
     ["ルビ辞書の重複をチェック"
      mto-rubyjisyo-kakunin t]
     ["関西弁辞書の重複をチェック"
      mto-kansaijisyo-kakunin t]
     ["ハングル辞書の重複をチェック"
      mto-hangeuljisyo-kakunin t])
    ))

(if (eql mto-menu-lang 'ja)
    (easy-menu-define mto-menu mto-mode-map "mto" mto-mode-menu-spec-ja)
  (easy-menu-define mto-menu mto-mode-map "mto" mto-mode-menu-spec-en))

(provide 'mto-menu)

;;; mto-menu.el ends here
