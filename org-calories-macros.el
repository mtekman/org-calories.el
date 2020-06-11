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
         (year (or year (string-to-number
                         (format-time-string "%Y"))))
         (month (or month (string-to-number
                           (format-time-string "%m"))))
         (tblym (format "#+NAME:%4d-%02d-Macros" year month)))
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (goto-char 0)
      (unless (search-forward tblym nil t)
        (user-error "Macros table %4d-%02d-Macros not available"
                    year month))
      (forward-line 1)
      (dolist (line (cddr (org-table-to-lisp)) marcs)
        (let ((type (intern (format ":%s" (downcase (car line)))))
              (min (string-to-number (nth 1 line)))
              (max (string-to-number (nth 2 line))))
          (setq marcs
                (plist-put marcs type
                           (list :min min :max max))))))))
;;

(defun org-calories-macros--summarize (scaled-items &optional groupday)
  "Summarize the list of SCALED-ITEMS into total nutritients, and calories.
If GROUPDAY, then summarize by day."
  (let ((summary nil)
        (daylist (-uniq (--map (plist-get it :date) scaled-items))))
    (dolist (day daylist summary) ;; always return a list of lists
      (let* ((scaled-items-day (--filter (or (not day)
                                             (string= (plist-get it :date) day))
                                         scaled-items))
             (foods (--filter (eq (plist-get it :type) 'food) scaled-items-day))
             (recipes (--filter (eq (plist-get it :type) 'recipe) scaled-items-day))
             (exercises (--filter (eq (plist-get it :type) 'exercise) scaled-items-day)))
        (let* ((flatten-foods (--reduce (org-calories-entry--foods-add acc it)
                                        (append foods recipes)))
               (_flatexers (--reduce (+ acc it)
                                     (if exercises
                                         (--map (plist-get it :kc) exercises)
                                       '(0))))
               (flatten-exers (unless (eq _flatexers 0) (list :exercise _flatexers))))
          (push (append (list :date day) flatten-foods flatten-exers) summary))))))


(defun org-calories-macros--collect (&optional year month day)
  "Collect scaled nutrional values for YEAR MONTH and DAY.
If DAY is t, then it collects the entire month.  If nil it collects the current day."
  (org-calories-log--makeheaders)
  (let* ((year (or year (string-to-number (format-time-string "%Y"))))
         (month (or month (string-to-number (format-time-string "%m"))))
         (day (or day (string-to-number (format-time-string "%d"))))
         (tblym (format "#+NAME:%4d-%02d-Logbook" year month))
         (timepref (if (eq day t)
                       (format "%4d-%02d" year month)  ;; entire month
                     (format "%4d-%02d-%02d" year month day))))
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (save-excursion
        (goto-char 0)
        (unless (search-forward tblym nil t)
          (user-error "Macros table %4d-%02d-Logbook not available" year month))
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
                                      (plist-get timedata :day-start)))
                     (timestamp (format "<%s>" timekey)))
                (if (string-prefix-p timepref timekey)
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
                                   amount))
                                 (t (user-error "No such type")))))
                      (push (append (list :date timestamp :name item :type type) amount-scaled)
                            list-items)))))))))))


(defun org-calories-macros--tableretrieve (year month)
  "Retrieve data from YEAR MONTH Dailies table."
  (org-calories-log--makeheaders)
  (let ((tabdata nil))
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (if (search-forward (format "#+NAME:%4d-%02d-Dailies" year month) nil t)
          (let* ((table-data (org-table-to-lisp))
                 (header-order (--map (if (string-prefix-p ":" it) (intern it))
                                      (car (org-table-to-lisp)))))
            (dolist (row (cddr table-data) tabdata)
              (let ((paired-data (org-calories-db--parsetypes
                                  (--reduce-from (append acc it) nil
                                                 (--filter (car it) ;; discard non-keyword columns
                                                           (--zip-with (list it other)
                                                                       header-order
                                                                       row))))))
                (push paired-data tabdata))))))))


;; (defun org-calories-macros--tableupdate (year month &optional day)
;;   "Update the Dailies table for YEAR MONTH  DAY if given, otherwise for all dates."
;;   (org-calories-log--makeheaders)
;;   (let ((year (or year (string-to-number (format-time-string "%Y"))))
;;         (month (or month (string-to-number (format-time-string "%m"))))
;;         (day (or day (string-to-number (format-time-string "%d")))))
;;     (with-current-buffer (find-file-noselect org-calories-log-file)
;;       (if (search-forward (format "#+NAME:%4d-%02d-Dailies" year month) nil t)
;;           (let ((header-order (--map (intern it) (car (org-table-to-lisp)))))
;;             (org-calories-db--kill-table)
;;             (setf (buffer-substring (line-beginning-position) (line-end-position)) "")
;;             ;; Dump current database
;;             ;; TODO: FIX THIS
;;             (dolist (entry
;;                      (--map (cons :name it)
;;                             (--sort ;; Need to get date.
;;                      (org-calories-macros--collect year month day)

;;                      (--map (cons :name it)
;;                                   (--sort (string-lessp (car it)(car other)) (symbol-value dbsymbl)))) ;; rows
;;               (dolist (keyw header-order)                                    ;; columns
;;                 (let ((am (plist-get entry keyw)))
;;                   (insert (format (if (floatp am) "| %.1f " "| %s ") am))))
;;               (insert "|\n")))  ;;(org-table-next-field))))
;;         (when (re-search-backward org-table-line-regexp nil t)
;;           (org-table-align))))))



;;     (unless (org-calories-entry--foods-retrieve food)
;;       (if (y-or-n-p (format "Food '%s' does not exist, insert new? " food))
;;           (let ((newinfo (org-calories-entry-foods-insert food)))
;;             (setq food (car newinfo)
;;                   food-info (cdr newinfo)))))
;;     ;;
;;     (let* ((food-info (org-calories-entry--foods-retrieve food))
;;            (amount-want (or portion (read-number (message "[%s] -- %s\nWhat portion of food (g)? "
;;                                                           food food-info))))
;;            (scaled-food (org-calories-db--scale-item food-info amount-want))
;;            (scaled-calories (plist-get scaled-food :kc)))
;;       (org-calories-log--goto-tableend)
;;       (org-calories-log--insert 'food food amount-want scaled-calories)
;;       (org-calories-db--trimandsort t)
;;       (save-buffer))))



(provide 'org-calories-macros)
;;; org-calories-macros.el ends here
