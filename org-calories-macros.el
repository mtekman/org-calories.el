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
      (dolist (line (cddr (org-table-to-lisp)) marcs)
        (let ((type (intern (format ":%s" (downcase (car line)))))
              (min (string-to-number (nth 1 line)))
              (max (string-to-number (nth 2 line))))
          (setq marcs
                (plist-put marcs type
                           (list :min min :max max))))))))
;;

(defun org-calories-macros--summarize (scaled-items)
  "Summarize the list of SCALED-ITEMS into total nutritients, and calories"
  (--reduce (+ acc it) (--map (plist-get it :kc) scaled-items)))


(defun org-calories-macros--collect (&optional year month day)
  "Collect scaled nutrional values for YEAR MONTH and DAY."
  (org-calories-log--makeheaders)
  (let* ((myear (or year (string-to-number
                          (format-time-string "%Y"))))
         (mmont (or month (string-to-number
                           (format-time-string "%m"))))
         (mday (or day (string-to-number
                        (format-time-string "%d"))))
         (tblym (format "#+NAME:%4d-%02d-Logbook" myear mmont))
         (curdat (format "%4d-%02d-%02d" myear mmont mday)))
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (goto-char 0)
      (unless (search-forward tblym nil t)
        (user-error "Macros table %4d-%02d-Logbook not available" myear mmont))
      (forward-line 1)
      (let ((list-items nil))
        (dolist (line (cddr (org-table-to-lisp)) list-items)
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
                  (let ((amount-scaled
                         (cond ((string= type "food")
                                (org-calories-db--scale-item
                                 (org-calories-entry--foods-retrieve item)
                                 amount))
                               ((string= type "recipe")
                                (org-calories-db--scale-item
                                 (org-calories-entry--recipes-calculate
                                  (org-calories-entry--recipes-retrieve item))
                                 amount))
                               ((string= type "exercise")
                                (org-calories-db--scale-item
                                 (org-calories-entry--exercises-retrieve item)
                                 (- amount))) ;; we negate exercise contributions
                               (t (user-error "No such type")))))
                    (push (append (list :name item :type type) amount-scaled)
                          list-items))))))))))

(provide 'org-calories-macros)
;;; org-calories-macros.el ends here
