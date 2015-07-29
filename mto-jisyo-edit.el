;;; mto-jisyo-edit.el --- jisyo highlighter

;; Author: nakinor
;; Created: 2015-07-29
;; Revised: 2015-07-29

;;; Commentary:

;;; Code:

(defun mto-jisyo-edit ()
  "highlighting jisyo"
  (interactive)
  (defface mto-jisyo-spacer '((t (:foreground "sky blue" t))) nil)
  (defface mto-jisyo-ignore '((t (:foreground "sandy brown" t))) nil)
  (defface mto-jisyo-separater '((t (:foreground "maroon" t))) nil)
  (defface mto-jisyo-comment '((t (:foreground "lightgreen" t))) nil)
  (defvar mto-jisyo-spacer 'mto-jisyo-spacer)
  (defvar mto-jisyo-ignore 'mto-jisyo-ignore)
  (defvar mto-jisyo-separater 'mto-jisyo-separater)
  (defvar mto-jisyo-comment 'mto-jisyo-comment)

  (font-lock-add-keywords nil
                          '(("^;-----*" . 'mto-jisyo-spacer)
                            ("^;.*" . 'mto-jisyo-ignore)
                            ("/" . 'mto-jisyo-separater)
                            (" ;.*" . 'mto-jisyo-comment)
                            )))

(provide 'mto-jisyo-edit)

;;; mto-jisyo-edit.el ends here
