(require 'org-calories-database)

(setq logbookfile "~/logbook.org")

(setq str-ltitled "#+TITLE: Daily Logs"
      str-targets "*** Targets / Macros"
      hed-targets "| Type | Daily [Min,Max] | Weekly [Min,Max] | Monthly [Min,Max] |"
      str-daylogs "*** Logs"
      hed-daylogs "| Timestamp | Type | Item | Amount(g) | Calories(kC) |")

(defun logbook-makeheaders ()
  "Make table headers."
  (let ((hed-year (format-time-string "* %Y"))
        (hed-month (format-time-string "** %m - %b"))
        (tbl-macros (format-time-string "%Y-%m-Macros"))
        (tbl-logs (format-time-string "%Y-%m-Logbook")))
    (with-current-buffer (find-file-noselect logbookfile)
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
          (database-maketable str-targets tbl-macros hed-targets))
        ;; Search for logs table
        (unless (search-forward tbl-logs nil t)
          (database-maketable str-daylogs tbl-logs hed-daylogs))))))


(defun logbook-completions (type)
  "Produce completion keys for TYPE."
  (database-generate type)
  (--map (car it)
         (cond ((eq type 'foods) db-foods)
               ((eq type 'recipes) db-recipes)
               ((eq type 'exercises) db-exercises)
               (t (user-error "Not an option")))))

(defun logbook-goto-tableend ()
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

(defun logbook-log-insert (type item amount calories)
  "In the logbook insert a row with TYPE, ITEM, AMOUNT, and CALORIES."
  (org-insert-time-stamp (current-time) t)
  (org-table-next-field)
  (insert (capitalize (format "%s" type)))
  (org-table-next-field)
  (insert item)
  (org-table-next-field)
  (insert (format "%d" amount))
  (org-table-next-field)
  (insert (format "%d" calories))
  (org-table-next-field))

(defun logbook-log-food (food)
  "Log FOOD entry."
  (interactive
   (list (completing-read "Food: "
                          (logbook-completions 'foods))))
  (let ((food-entry (db-foods-retrieve food))
        (tbl-macros (format-time-string "#+NAME:%Y-%m-Macros"))
        (tbl-logs (format-time-string "#+NAME:%Y-%m-Logbook")))
    (unless food-entry
      (if (y-or-n-p (format "Food '%s' does not exist, insert new food? "
                            food))
          (db-foods-insert food)))
    (logbook-makeheaders)
    (with-current-buffer (find-file-noselect logbookfile)
      (goto-char 0)
      (unless (search-forward tbl-logs nil t)
        (user-error "Could not find table %s.  Please check your logbook"
                    tbl-logs))
      ;; At first empty
      (let* ((amount (read-number (message (format
                     "[%s] -- %s\nWhat portion of food (g)? "
                     food
                     (db-foods-retrieve food)))))
             (scaled-food (db-scale-item 'foods food-entry amount))
             (scaled-calories (plist-get scaled-food :kc)))
        ;; Currently at table head
        (logbook-goto-tableend)
        (logbook-log-insert 'food food amount scaled-calories)
        (database-trimandsort)
        (save-buffer)))))

(defun logbook-log-recipe (recipe &optional portion)
  "Log RECIPE entry with optional PORTION."
  (interactive
   (list (completing-read "Recipe: " (logbook-completions 'recipes))))
  (let ((recipe-entry (db-recipes-retrieve recipe))
        (tbl-macros (format-time-string "#+NAME:%Y-%m-Macros"))
        (tbl-logs (format-time-string "#+NAME:%Y-%m-Logbook")))
    (unless recipe-entry
      (if (y-or-n-p (format "Recipe '%s' does not exist, insert new Recipe? "
                            recipe))
          (db-recipes-insert recipe)))
    (logbook-makeheaders)
    (with-current-buffer (find-file-noselect logbookfile)
      (goto-char 0)
      (unless (search-forward tbl-logs nil t)
        (user-error "Could not find table %s.  Please check your logbook"
                    tbl-logs))
      (let* ((recipe-info (db-recipes-retrieve recipe))
             (amount-want (or portion
                              (read-number
                               (message (format
                                         "[%s] -- %s\nWhat amount of recipe? "
                                         recipe recipe-info)))))
             ;; the above has portion value of -1, but is the total
             ;; food value of the native portion of that recipe
             (calced-recipe (db-recipes-calculate recipe-info))
             (scaled-recipe (db-scale-item 'recipes calced-recipe amount-want))
             (scaled-calories (plist-get scaled-recipe :kc)))
        ;; Currently at table head
        (logbook-goto-tableend)
        (logbook-log-insert 'recipes recipe amount-want scaled-calories)
        (database-trimandsort)
        (save-buffer)))))

(defun logbook-log-exercise (exercise &optional duration)
  "Log EXERCISE entry with optional DURATION."
  (interactive
   (list (completing-read "Exercise: " (logbook-completions 'exercises))))
  (let ((exercise-entry (db-exercises-retrieve exercise))
        (tbl-macros (format-time-string "#+NAME:%Y-%m-Macros"))
        (tbl-logs (format-time-string "#+NAME:%Y-%m-Logbook")))
    (unless exercise-entry
      (if (y-or-n-p (format "Exercise '%s' does not exist, insert new Exercise? "
                            exercise))
          (db-exercises-insert exercise)))
    (logbook-makeheaders)
    (with-current-buffer (find-file-noselect logbookfile)
      (goto-char 0)
      (unless (search-forward tbl-logs nil t)
        (user-error "Could not find table %s.  Please check your logbook"
                    tbl-logs))
      (let* ((exercise-info (db-exercises-retrieve exercise))
             (amount-want (or duration
                              (read-number
                               (message (format
                                         "[%s] -- %s\nWhat amount of exercise? "
                                         exercise exercise-info)))))
             (scaled-exercise (db-scale-item 'exercises exercise-info amount-want))
             (scaled-calories (plist-get scaled-exercise :kc)))
        ;; Currently at table head
        (logbook-goto-tableend)
        (logbook-log-insert 'exercises exercise amount-want scaled-calories)
        (database-trimandsort)
        (save-buffer)))))


(provide 'org-calories-logbook)
;;; org-calories-logbook.el ends here
