;;; org-calories-log.el --- Daily logging functions for org-calories -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

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
(require 'org-table)
(require 'org-calories-db)

(defcustom org-calories-log-file "~/logbook.org")

;; Strings
(defconst str-ltitled "#+TITLE: Daily Logs")
(defconst str-targets "*** Targets / Macros")
(defconst hed-targets "| Dailies | Min | Max |")
(defconst str-daylogs "*** Logs")
(defconst hed-daylogs "| Timestamp | Type | Item | Amount | Calories(kC) |")

(defun org-calories-log--makeheaders ()
  "Make table headers."
  (let ((hed-year (format-time-string "* %Y"))
        (hed-month (format-time-string "** %m - %b"))
        (tbl-macros (format-time-string "%Y-%m-Macros"))
        (tbl-logs (format-time-string "%Y-%m-Logbook")))
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (save-excursion
        (goto-char 0)
        ;; Make Title if not found
        (unless (search-forward str-ltitled nil t)
          (insert str-ltitled)
          (insert "\n\n"))
        ;; Search for Year
        (if (search-forward hed-year nil t)
            (unless (search-forward hed-month nil t)
              ;; insert just month
              (insert (format "\n%s\n" hed-month)))
          ;; otherwise insert year and month at end of buffer
          (goto-char (point-max))
          (insert (format "\n%s\n%s\n" hed-year hed-month)))
        ;; Search for headers
        ;; Search for macros table
        (unless (search-forward tbl-macros nil t)
          (org-calories-db--maketable str-targets tbl-macros hed-targets)
          (forward-line -2)
          (setf (buffer-substring (line-beginning-position) (line-end-position)) "")
          (dolist (var '(kC Carbs Fibre Sugars Protein Fat Exercise Water))
            (insert (format "| %s\n" var)))
          (forward-line -1)
          (org-table-align))
        ;; Search for logs table
        (unless (search-forward tbl-logs nil t)
          (org-calories-db--maketable str-daylogs tbl-logs hed-daylogs))))))

(defun org-calories-log--completions (type)
  "Produce completion keys for TYPE."
  (org-calories-db--generate type)
  (--map (car it)
         (cond ((eq type 'foods) db-foods)
               ((eq type 'recipes) db-recipes)
               ((eq type 'exercises) db-exercises)
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
  (insert (format "%d" calories)) ;; fractional calories would be pointless here
  (org-table-next-field))

(defun org-calories-log--prelog (type name)
  "Preamble for importing NAME of TYPE, and finding the right table."
  (let* ((captype (capitalize (format "%s" type)))
         (funcretr (intern (format "db-%s-retrieve" type)))
         (funcinst (intern (format "db-%s-insert" type)))
         (type-entry (funcall funcretr name))
         (tbl-macros (format-time-string "#+NAME:%Y-%m-Macros"))
         (tbl-logs (format-time-string "#+NAME:%Y-%m-Logbook")))
    (unless type-entry
      (if (y-or-n-p (format "%s '%s' does not exist, insert new %s? "
                            captype name captype))
          (funcall funcinst name)))
    (org-calories-log--makeheaders)
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (goto-char 0)
      (unless (search-forward tbl-logs nil t)
        (user-error "Could not find table %s.  Please check your logbook"
                    tbl-logs)))))


(defun org-calories-log-food (food &optional portion)
  "Log FOOD entry with PORTION."
  (interactive
   (list (completing-read "Food: "
                          (org-calories-log--completions 'foods))))
  (org-calories-log--prelog 'foods food)
  ;; At first empty
  (let* ((food-info (db-foods-retrieve food))
         (amount-want (or portion (read-number (message (format
                                  "[%s] -- %s\nWhat portion of food (g)? "
                                  food food-info)))))
         (scaled-food (db-scale-item 'foods food-info amount-want))
         (scaled-calories (plist-get scaled-food :kc)))
    ;; Currently at table head
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (org-calories-log--goto-tableend)
      (org-calories-log--insert 'foods food amount-want scaled-calories)
      (org-calories-db--trimandsort)
      (save-buffer))))

(defun org-calories-log-recipe (recipe &optional portion)
  "Log RECIPE entry with optional PORTION."
  (interactive
   (list (completing-read "Recipe: " (org-calories-log--completions 'recipes))))
  (org-calories-log--prelog 'recipes recipe)
  (let* ((recipe-info (db-recipes-retrieve recipe))
         (amount-want (or portion (read-number (message (format
                                  "[%s] -- %s\nWhat amount of recipe? "
                                  recipe recipe-info)))))
         ;; the above has portion value of -1, but is the total
         ;; food value of the native portion of that recipe
         (calced-recipe (db-recipes-calculate recipe-info))
         (scaled-recipe (db-scale-item 'recipes calced-recipe amount-want))
         (scaled-calories (plist-get scaled-recipe :kc)))
    ;; Currently at table head
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (org-calories-log--goto-tableend)
      (org-calories-log--insert 'recipes recipe amount-want scaled-calories)
      (org-calories-db--trimandsort)
      (save-buffer))))

(defun org-calories-log-exercise (exercise &optional amount)
  "Log EXERCISE entry with optional AMOUNT.
The unit does not actually matter because it's set by the database and we are just scaling it."
  (interactive
   (list (completing-read "Exercise: " (org-calories-log--completions 'exercises))))
  (org-calories-log--prelog 'exercises exercise)
  (let* ((exercise-info (db-exercises-retrieve exercise))
         (amount-want (or amount
                          (read-number (message (format
                           "[%s] -- %s\nWhat amount of exercise? "
                           exercise exercise-info)))))
         (scaled-exercise (db-scale-item 'exercises exercise-info amount-want))
         (scaled-calories (plist-get scaled-exercise :kc)))
    ;; Currently at table head
    (with-current-buffer (find-file-noselect org-calories-log-file)
      (org-calories-log--goto-tableend)
      (org-calories-log--insert 'exercises exercise amount-want scaled-calories)
      (org-calories-db--trimandsort)
      (save-buffer))))


(provide 'org-calories-log)
;;; org-calories-log.el ends here
