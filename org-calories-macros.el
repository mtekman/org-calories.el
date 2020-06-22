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

(defun org-calories-macros--get ()
  "Get latest target macros."
  (org-calories-log--makeheaders)
  (let ((marcs nil)
        (tblym "#+NAME:Macros")
        (latest (lambda (data type)
                  (--reduce
                   (< (car (org-read-date nil t (plist-get acc :timestamp)))
                      (car (org-read-date nil t (plist-get it :timestamp))))
                   (--filter (string= type (plist-get it :type)) data)))))
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (goto-char 0)
      (unless (search-forward tblym nil t)
        (user-error "Macros table not available"))
      (forward-line 1)
      (let* ((tabledata (org-table-to-lisp))
             (header (car tabledata))
             (tdata (cddr tabledata)))
        (dolist (line tdata marcs)
          (let ((ldata (org-calories-db--pairtypes header line)))
            (push ldata marcs)))
        ;; Filter for latest daily and weekly
        (let ((daily (funcall latest marcs "Daily"))
              (weekly (funcall latest marcs "Weekly")))
          (ignore weekly)
          daily)))))


(defun org-calories-macros--summarize (scaled-items &optional groupday)
  "Summarize the list of SCALED-ITEMS into total nutritients, and calories.
If GROUPDAY, then summarize by day."
  (ignore groupday)
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
               ;; If it's a single item, check again
               (flatten-foods (if (plist-get flatten-foods :name)
                                  (org-calories-entry--foods-add flatten-foods nil)
                                flatten-foods))
               (flatexers (--reduce (+ acc it)
                                    (if exercises
                                        (--map (plist-get it :kc) exercises)
                                      '(0))))
               (flatten-exers (unless (eq flatexers 0) (list :exercise flatexers))))
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
      (goto-char 0)
      (when (search-forward (format "#+NAME:%4d-%02d-Dailies" year month) nil t)
        (forward-line 1)
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



(defun org-calories-macros--timestring-to-integers (timestr)
  "Convert TIMESTR <2020-06-14> or <2020-06-14 Wed 16:15> to list (2020 6 14 16 15)."
  (--filter (> it 0)(--map (string-to-number it)
                           (split-string timestr "[-<>: ]"))))

(defun org-calories-macros--timestring-lteq (timelst1 timelst2)
  "TIMELST1 less than or equal to TIMELST2?"
  (and
   (and (<= (car timelst1) (car timelst2))        ;; year
        (<= (cadr timelst1) (cadr timelst2))      ;; month
        (<= (caddr timelst1) (caddr timelst2)))   ;; day
   (if (eq (length timelst1) 3) t
     (and (<= (cadddr timelst1) (cadddr timelst2))                 ;; hour
          (<= (cadddr (cdr timelst1)) (cadddr (cdr timelst2))))))) ;; minute

(defun org-calories-macros--timestring-eq (timelst1 timelst2)
  "TIMELST1 equal to TIMELST2?"
  (and
   (and (= (car timelst1) (car timelst2))        ;; year
        (= (cadr timelst1) (cadr timelst2))      ;; month
        (= (caddr timelst1) (caddr timelst2)))   ;; day
   (if (eq (length timelst1) 3) t
     (and (= (cadddr timelst1) (cadddr timelst2))                 ;; hour
          (= (cadddr (cdr timelst1)) (cadddr (cdr timelst2))))))) ;; minute



(defun org-calories-macros--tableupdate (year month &optional day)
  "Update the Dailies table for YEAR MONTH  DAY if given, otherwise for all dates."
  ;; TODO: Slow when day is true.
  (org-calories-log--makeheaders)
  (let* ((year (or year (string-to-number (format-time-string "%Y"))))
         (month (or month (string-to-number (format-time-string "%m"))))
         (day (or day (string-to-number (format-time-string "%d"))))
         (current-dailies (org-calories-macros--tableretrieve year month))
         (header-order (--filter (symbolp it) (car current-dailies)))
         ;; Parse the logbook
         (new-daylist (org-calories-macros--summarize
                       (org-calories-macros--collect year month day))))
    ;; Update new-daylist into current-dailies
    (dolist (day new-daylist current-dailies)
      (let* ((new-date (plist-get day :date))
             (new-day-num (org-calories-macros--timestring-to-integers new-date))
             ;; subtract exercises from kc
             (day (plist-put day :kc (- (plist-get day :kc)
                                        (or (plist-get day :exercise) 0))))
             ;; do a string match on dates
             (ind-date (--find-index (org-calories-macros--timestring-eq
                                      new-day-num
                                      (org-calories-macros--timestring-to-integers
                                       (plist-get it :date)))
                                     current-dailies)))
        (if ind-date
            ;; Update existing
            (let* ((get-date (nth ind-date current-dailies))
                   (notes (plist-get get-date :notes))           ;; keep existing notes
                   (new-day (append day (list :notes (or notes "")))))
              (setq current-dailies
                    (-replace-at ind-date new-day current-dailies)))
          ;; Otherwise insert new date at the right location
          (let ((insert-loc (--find-index (org-calories-macros--timestring-lteq
                                           new-day-num
                                           (org-calories-macros--timestring-to-integers
                                            (plist-get it :date)))
                                          current-dailies))
                (new-day (append day (list :notes ""))))
            (if insert-loc
                (setq current-dailies (-insert-at insert-loc new-day current-dailies))
              (setq current-dailies (append current-dailies
                                            ;; new day with new empty notes
                                            (list new-day))))))))
    ;; Print out the whole table
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (goto-char 0)
      (when (search-forward (format "#+NAME:%4d-%02d-Dailies" year month) nil t)
        (forward-line 1)
        (org-calories-db--kill-table)
        (setf (buffer-substring (line-beginning-position) (line-end-position)) "")
        ;; Process rows
        (dolist (entry (reverse current-dailies))
          (dolist (keyw header-order)
            (let* ((am (plist-get entry keyw))
                   (am (or am 0)))
              (insert (format (if (floatp am) "| %.1f " "| %s ") am))
              (when (eq keyw :date)
                (forward-whitespace -1)
                (forward-char -1)
                (org-timestamp-up-day)
                (org-timestamp-down-day)
                (forward-char 1)
                (forward-whitespace 1))))
          (insert "|\n")))
      (if (re-search-backward org-table-line-regexp nil t)
          (org-table-align)))))


(defun org-calories-macros-printlast ()
  "Print the last nutrients."
  (interactive)
  (message "%s" (cddar (org-calories-macros--summarize
                        (org-calories-macros--collect)))))

(add-hook 'org-calories-log-finishhook #'org-calories-macros-printlast)


(provide 'org-calories-macros)
;;; org-calories-macros.el ends here
