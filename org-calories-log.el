;;; org-calories-log.el --- Daily logging functions for org-calories -*- lexical-binding: t; -*-

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
(require 'org)
(require 'org-table)
(require 'org-calories-db)
(require 'org-calories-entry)
(require 'org-calories-timestring)

(defcustom org-calories-log-file nil
  "Location of daily logging file."
  :group 'org-calories
  :type 'string)

(defcustom org-calories-log-headers-macros
  '(:kc :fat :sat :carbs :sugars :fibre :protein :exercise :water)
  "Headers to use when creating macro tables."
  :group 'org-calories
  :type 'list)

(defcustom org-calories-log-headers-dailies
  '(:kc :fat :sat :carbs :sugars :fibre :protein :salt :exercise :water)
  "Headers to use when creating dailies tables."
  :group 'org-calories
  :type 'list)

(defcustom org-calories-log-headers-logbook
  '(:type :item :amount)
  "Headers to use when creating logbook tables."
  :group 'org-calories
  :type 'list)

(defvar org-calories-log-finishhook nil
  "Finish hook.")

;;(setq minibuffer-local-filename-completion-map nil) ;; prevents space from being a keyword during completion

;; Strings
(defconst org-calories-log-str-ltitled "#+TITLE: Daily Logs")
(defconst org-calories-log-str-targets "*** Targets / Macros")
(defconst org-calories-log-hed-targets "| Dailies | Min | Max |")
(defconst org-calories-log-str-daylogs "*** Logs")

(defcustom org-calories-log-showsummary t
  "Show a summary after logging."
  :type 'boolean
  :group 'org-calories)

(defun org-calories-log ()
  "Log new entry."
  (interactive)
  (let* ((type (read-multiple-choice
                "Entry Type: " '((?f "food")
                                 (?r "recipe")
                                 (?e "exercise"))))
         (fstr (format "org-calories-log-%s" (cadr type)))
         (fint (intern fstr)))
    (call-interactively fint)))

(defun org-calories-log-show ()
  "Show the log file."
  (interactive)
  (find-file org-calories-log-file))

(defun org-calories-log--makeheaders ()
  "Make table headers."
  (cl-labels ((tblheader (beg middlelist end)
                         (concat beg (--reduce (format "%s | %s" acc it) middlelist) end))
              (maketable (header tblname dstring header-list)
                         (org-calories-db--maketable
                          header tblname
                          (tblheader dstring header-list " |"))))
    (let ((hed-year (format-time-string "* %Y"))
          (hed-month (format-time-string "** %m - %b"))
          (tbl-logs (format-time-string "%Y-%m-Logbook"))
          (tbl-dail (format-time-string "%Y-%m-Dailies")))
      (with-current-buffer (find-file-noselect org-calories-log-file)
        (save-excursion
          (goto-char 0)
          ;; Make Title if not found
          (unless (search-forward org-calories-log-str-ltitled nil t)
            (insert org-calories-log-str-ltitled)
            (insert "\n\n"))
          ;; Search for Macros
          (unless (search-forward "* Macros" nil t)
            (maketable "* Macros" "Macros" "| :timestamp | " org-calories-log-headers-macros))
          ;; Search for Year, Month
          (unless (search-forward hed-year nil t)
            (insert (format "\n%s\n" hed-year)))
          (unless (search-forward hed-month nil t)
            (insert (format "\n%s\n" hed-month)))
          ;; Search for tables
          (unless (search-forward (concat "#+NAME:" tbl-dail) nil t)
            (insert "\n\n")
            (maketable nil tbl-dail "| :date | " org-calories-log-headers-dailies))
          (unless (search-forward (concat "#+NAME:" tbl-logs) nil t)
            (insert "\n\n")
            (maketable nil tbl-logs "| :timestamp | " (append org-calories-log-headers-logbook (list '~KC)))))))))

(defun org-calories-log--completions (type)
  "Produce completion keys for TYPE."
  (org-calories-db--generate type)
  (--map (car it)
         (cond ((eq type 'foods) org-calories-db--foods)
               ((eq type 'recipes) org-calories-db--recipes)
               ((eq type 'exercises) org-calories-db--exercises)
               (t (user-error "Not an option")))))

(defun org-calories-log--goto-tableend ()
  "Jump to end of table for insertion."
  (forward-line 1)
  (goto-char (org-table-end))
  (let* ((lbeg (line-beginning-position))
         (lend (line-end-position))
         (line-contents (string-trim (buffer-substring-no-properties
                                      lbeg lend))))
    (if (string= "|" line-contents) ;; wipe line
        (setf (buffer-substring lbeg lend) "")
      (insert "| "))))

(defun org-calories-log--insert (type item amount calories)
  "In the logbook insert a row with TYPE, ITEM, AMOUNT, and CALORIES."
  (org-insert-time-stamp (current-time) t)
  (org-table-next-field)
  (insert (capitalize (format "%s" type)))
  (org-table-next-field)
  (insert item)
  (org-table-next-field)
  (insert (format (if (floatp amount) "%.2f" "%d") amount))
  (org-table-next-field)
  (insert (format (if (floatp calories) "%.2f" "%d") calories))
  (org-table-next-field))


(defun org-calories-log--prelog ()
  "Preamble for importing NAME of TYPE, and finding the right table."
  (let (;;(tbl-macros (format-time-string "#+NAME:%Y-%m-Macros"))
        (tbl-logs (format-time-string "#+NAME:%Y-%m-Logbook")))
    (org-calories-log--makeheaders)
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (goto-char 0)
      (unless (search-forward tbl-logs nil t)
        (user-error "Could not find table %s.  Please check your logbook"
                    tbl-logs)))))

(defun org-calories-log-runfinish ()
  "Run log finish functions."
  (message nil)
  (save-buffer)
  (if org-calories-log-showsummary
      (run-hooks 'org-calories-log-finishhook)))

(defun org-calories-log--endlog (type name amount kc)
  "Function that occurs after logging.
Inserts TYPE NAME AMOUNT KC, sorts and trims the table."
  (save-excursion
    (org-calories-log--goto-tableend)
    (org-calories-log--insert type name amount kc)
    (org-calories-db--trimandsort t)
    (org-calories-log-runfinish)))

(defun org-calories-log-food (food &optional portion)
  "Log FOOD entry with PORTION."
  (interactive
   (list (completing-read "Food: " (org-calories-log--completions 'foods))))
  ;; TODO: Override space
  (org-calories-log--prelog)
  ;; Currently at table head
  (with-current-buffer (find-file-noselect org-calories-log-file)
    ;; At first empty
    (let ((food-info (org-calories-entry--foods-retrieve food)))
      (unless food-info
        (if (y-or-n-p (format "Food '%s' does not exist, insert new? " food))
            (let ((newinfo (org-calories-entry-foods-insert food)))
              (setq food (car newinfo)
                    food-info (cdr newinfo)))))
      ;;
      (let* ((amount-want (or portion (read-number (message "[%s] -- %s\nWhat portion of food (g)? "
                                                            food food-info))))
             (scaled-food (org-calories-db--scale-item food-info amount-want))
             (scaled-calories (plist-get scaled-food :kc)))
        (org-calories-log--endlog 'food food amount-want scaled-calories)))))


(defun org-calories-log-note (&optional date)
  "Log note for DATE."
  (interactive)
  (let* ((tabdt nil)
         (daten (or date (org-calories-timestring--to-integers (org-read-date))))
         (tabld (format "#+NAME:%4d-%02d-Notes" (car daten) (cadr daten)))
         (datem (--reduce (* acc it) daten))
         (bas64 (base64-encode-string (format "%s" datem))))
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (goto-char 0)
      (when (search-forward tabld nil t)
        (forward-line 1)
        (let* ((table-data (org-table-to-lisp))
               (header-order (--map (if (string-prefix-p ":" it) (intern it))
                                    (car table-data))))
          (dolist (row (cddr table-data) tabdt)
            (let ((paired-data (--reduce-from (append acc it) nil
                                              (--filter (car it) ;; discard non-keyword columns
                                                        (--zip-with (list it other)
                                                                    header-order
                                                                    row)))))
              (push paired-data tabdt))))))))


(defun org-calories-log-recipe (recipe &optional portion)
  "Log RECIPE entry with optional PORTION."
  (interactive
   (list (completing-read "Recipe: " (org-calories-log--completions 'recipes))))
  (org-calories-log--prelog)
  ;; Currently at table head
  (with-current-buffer (find-file-noselect org-calories-log-file)
    ;;
    (unless (org-calories-entry--recipes-retrieve recipe)
      (if (y-or-n-p (format "Recipe '%s' does not exist, insert new? " recipe))
          (org-calories-entry-recipes-insert recipe)))
    ;;
    (let* ((recipe-info (org-calories-entry--recipes-retrieve recipe))
           (amount-want (or portion (read-number (message (format
                                                           "[%s] -- %s\nWhat amount of recipe? "
                                                           recipe recipe-info)))))
           ;; the above has portion value of -1, but is the total
           ;; food value of the native portion of that recipe
           (calced-recipe (org-calories-entry--recipes-calculate recipe-info))
           (scaled-recipe (org-calories-db--scale-item calced-recipe amount-want))
           (scaled-calories (plist-get scaled-recipe :kc)))
      (org-calories-log--endlog 'recipe recipe amount-want scaled-calories))))


(defun org-calories-log-exercise (exercise &optional amount)
  "Log EXERCISE entry with optional AMOUNT.
The unit does not actually matter because it's set by the database and we are just scaling it."
  (interactive
   (list (completing-read "Exercise: " (org-calories-log--completions 'exercises))))
  (org-calories-log--prelog)
  ;; Currently at table head
  (with-current-buffer (find-file-noselect org-calories-log-file)
    (unless (org-calories-entry--exercises-retrieve exercise)
      (if (y-or-n-p (format "Exercise '%s' does not exist, insert new? " exercise))
          (org-calories-entry-exercises-insert exercise)))
    ;;
    (let* ((exercise-info (org-calories-entry--exercises-retrieve exercise))
           (amount-want (or amount
                            (read-number (message (format
                                                   "[%s] -- %s\nWhat amount of exercise? "
                                                   exercise exercise-info)))))
           (scaled-exercise (org-calories-db--scale-item exercise-info amount-want))
           (scaled-calories (plist-get scaled-exercise :kc)))
      (org-calories-log--endlog 'exercise exercise amount-want
                                (- scaled-calories))))) ;; negative kc


;;TODO:
(defun org-calories-log-water (amount)
  "Log water AMOUNT."
  (ignore amount))
(defun org-calories-log-weight (amount)
  "Log weight AMOUNT."
  (ignore amount))


(provide 'org-calories-log)
;;; org-calories-log.el ends here
