;;; mto-vars.el --- variable utilities

;; Author: nakinor
;; Created: 2012-06-12
;; Revised: 2015-04-10

;;; Commentary:

;;; Code:

;;check the duplicated cons cell in association list
(defun check-dupulicated-alist (list)
  "連想リストの重複を調べて表示"
  (let ((independent-list '())
        (duplicated-list '())
        (duplicated-word ""))

    (mapcar (lambda (x)
              (if (member x independent-list)
                  (setq duplicated-list (cons x duplicated-list))
                (setq independent-list (cons x independent-list))))
            list)

    (mapcar (lambda (x)
              (setq duplicated-word
                    (concat (concat duplicated-word (car x)) ", ")))
            duplicated-list)

    (message "登録数: %d,  有効: %d,  重複: %d,  重複単語: %s"
             (length list)
             (length independent-list)
             (- (length list) (length independent-list))
             duplicated-word)))

(defun mto-kanajisyo-kakunin ()
  (interactive)
  (mto-parser mto-kanajisyo)
  (check-dupulicated-alist mto-alist))
(defun mto-kanjijisyo-kakunin ()
  (interactive)
  (mto-parser mto-kanjijisyo)
  (check-dupulicated-alist mto-alist))
(defun mto-checkjisyo-kakunin ()
  (interactive)
  (mto-parser mto-checkjisyo)
  (check-dupulicated-alist mto-alist))
(defun mto-rubyjisyo-kakunin ()
  (interactive)
  (mto-parser mto-rubyjisyo)
  (check-dupulicated-alist mto-alist))
(defun mto-kansaijisyo-kakunin ()
  (interactive)
  (mto-parser mto-kansaijisyo)
  (check-dupulicated-alist mto-alist))
(defun mto-hangeuljisyo-kakunin ()
  (interactive)
  (mto-parser mto-hangeuljisyo)
  (check-dupulicated-alist mto-alist))

(provide 'mto-vars)

;;; mto-vars.el ends here
