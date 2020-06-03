;;; org-calories-macros.el --- Calorific targets for org-calories -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-calories.el
;; Keywords: outlines
;; Package-Requires: ((emacs "26.1") (dash "2.17.0") (org "9.1.6"))
;; Version: 0.1

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; See org-calories.el

;;; Code:
(require 'org-calories-log)

(defun org-calories-macros--get (&optional year month)
  "Get daily target macros for YEAR and MONTH."
  (org-calories-log--makeheaders)
  (let* ((marcs nil)
         (myear (or year (string-to-number
                          (format-time-string "%Y"))))
         (mmont (or month (string-to-number
                           (format-time-string "%m"))))
         (tblym (format "#+NAME:%4d-%02d-Macros" myear mmont)))
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (goto-char 0)
      (unless (search-forward tblym nil t)
        (user-error "Macros table %4d-%02d-Macros not available"
                    myear mmont))
      (forward-line 1)
      (dolist (line (cddr (org-table-to-lisp)))
        (let ((type (intern (format ":%s" (downcase (car line)))))
              (min (string-to-number (nth 1 line)))
              (max (string-to-number (nth 2 line))))
          (setq marcs
                (plist-put marcs type
                           (list :min min :max max))))))
    marcs))
;;

(defun org-calories-macros--collect (&optional year month day)
  "Collect macros for YEAR MONTH and DAY."
  (org-calories-log--makeheaders)
  (let* ((myear (or year (string-to-number
                          (format-time-string "%Y"))))
         (mmont (or month (string-to-number
                           (format-time-string "%m"))))
         (tblym (format "#+NAME:%4d-%02d-Logbook" myear mmont))
         (curdat (format "%4d-%02d-%02d" year month day)))
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (goto-char 0)
      (unless (search-forward tblym nil t)
        (user-error "Macros table %4d-%02d-Logbook not available"
                    myear mmont))
      (forward-line 1)
      (let ((list-items nil))
        (dolist (line (cddr (org-table-to-lisp)))
          (let ((timestamp (nth 0 line))
                (type (intern (downcase (nth 1 line))))
                (item (nth 2 line))
                (amount (string-to-number (nth 3 line))))
            ;;(kc (string-to-number (nth 4 line))))
            (let* ((timedata (cadr (org-timestamp-from-string timestamp)))
                   (timekey (format "%4d-%02d-%02d"
                                    (plist-get timedata :year-start)
                                    (plist-get timedata :month-start)
                                    (plist-get timedata :day-start))))
              (if (string= timekey curdat)
                  ;; TODO: db-scale-item fails for recipes... we should
                  ;;       change the fn so that the food recursive parts
                  ;;       are performed inside the fn instead of outside
                  (let* ((func-name (intern (format "org-calories-entry--%s-retrieve" type)))
                         (item-details (funcall func-name item))
                         (amount-scaled (org-calories-db--scale-item "none" item-details amount)))
                    (push amount-scaled list-items))))))
        list-items))))

(provide 'org-calories-macros)
;;; org-calories-macros.el ends here

;; ((:amount 20 :unit g :kc 93 :carbs 15 :fibre 0 :sugars 4 :protein 1 :fat 3 :sodium 12)
;;  (:amount 1 :unit apple :kc 72 :carbs 19 :fibre 3 :sugars 14 :protein 0 :fat 0 :sodium 1)
;;  (:amount 40 :unit g :kc 186 :carbs 30 :fibre 0 :sugars 8 :protein 3 :fat 6 :sodium 24)
;;  (:amount 1 :ingredients ((:food Lime :amount 1) (:food Ginger raw :amount 50) (:food Carrot single :amount 3) (:food Celery :amount 3) (:food Kiwi :amount 1)))
;;  (:amount 1 :unit apple :kc 72 :carbs 19 :fibre 3 :sugars 14 :protein 0 :fat 0 :sodium 1)
;;  (:amount 1 :ingredients ((:food Protein Powder (Peanut Butter Flavour) :amount 180) (:food Apple :amount 2)))
;;  (:amount 2 :unit peach :kc 66 :carbs 16 :fibre 3 :sugars 13 :protein 2 :fat 0 :sodium 2))
