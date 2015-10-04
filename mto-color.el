;;; mto-color.el --- colorize replaced word

;; Author: nakinor
;; Created: 2012-06-25
;; Revised: 2015-10-04

;;; Commentary:

;;; Code:

;; 置換した文字列の装飾を設定
(defface kana-modern-face '((t (:foreground "green" :underline t))) nil)
(defface kana-trad-face '((t (:foreground "pink" :underline t))) nil)
(defface kanji-modern-face '((t (:foreground "green" t))) nil)
(defface kanji-trad-face '((t (:foreground "pink" t))) nil)
(defface mto-check-face '((t (:foreground "red" :bold t))) nil)
(defface mto-ruby-face '((t (:foreground "red"))) nil)
(defvar kana-modern-face 'kana-modern-face)
(defvar kana-trad-face 'kana-trad-face)
(defvar kanji-modern-face 'kanji-modern-face)
(defvar kanji-trad-face 'kanji-trad-face)
(defvar mto-check-face 'mto-check-face)
(defvar mto-ruby-face 'mto-ruby-face)

;; font-look に登録
(defun create-color-keywords-car (jisyo-name fl-face)
  "辞書ファイルの左にある単語をキーワードとして face に登録する (mto-key-car)"
  (setq mto-key-car-old mto-key-car)
  (with-temp-buffer
    (insert-file-contents jisyo-name)
    (mto-replace ";.*" "")
    (mto-replace " +$" "")
    (mto-replace "^\n" "")
    (mto-replace " /.*" (concat "\" . '" fl-face))
    (mto-replace "\n" ") (\"")
    (mto-replace "\n" "")
    (mto-replace " (\"+$" "")
    (mto-replace " (\")" "")
    (goto-char (point-min))
    (insert "(setq mto-key-car '((\"")
    (goto-char (point-max))
    (insert "))")
    (eval-buffer))
  (font-lock-add-keywords nil mto-key-car))

(defun create-color-keywords-cdr (jisyo-name fl-face)
  "辞書ファイルの右にある単語をキーワードとして face に登録する (mto-key-cdr)"
  (setq mto-key-cdr-old mto-key-cdr)
  (with-temp-buffer
    (insert-file-contents jisyo-name)
    (mto-replace ";.*" "")
    (mto-replace " +$" "")
    (mto-replace "^\n" "")
    (mto-replace "^.*/" "(\"")
    (mto-replace "\n" (concat "\" . '" fl-face ") "))
    (mto-replace " +$" "")
    (goto-char (point-min))
    (insert "(setq mto-key-cdr '(")
    (goto-char (point-max))
    (insert "))")
    (eval-buffer))
  (font-lock-add-keywords nil mto-key-cdr))

;; 色付けを解除するための手続
(setq mto-key-car '())
(setq mto-key-cdr '())
(defun mto-clear-color ()
  "色付けを解除するための実験。何だかとても遅くなってしまうのである"
  (interactive)
  (font-lock-mode 0)
  (if (boundp 'mto-key-car)
      (font-lock-remove-keywords nil mto-key-car))
  (if (boundp 'mto-key-cdr)
      (font-lock-remove-keywords nil mto-key-cdr))
  (if (boundp 'mto-key-car-old)
      (font-lock-remove-keywords nil mto-key-car-old))
  (if (boundp 'mto-key-cdr-old)
      (font-lock-remove-keywords nil mto-key-cdr-old))
  (font-lock-mode 1))
;(global-set-key (kbd "\C-c u") 'mto-clear-color)

(provide 'mto-color)

;;; mto-color.el ends here
