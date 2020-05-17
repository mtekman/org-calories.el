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



(provide 'org-calories-logbook)
;;; org-calories-logbook.el ends here
