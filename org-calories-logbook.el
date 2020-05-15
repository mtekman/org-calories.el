(setq logbookfile "~/logbook.org")

(setq str-ltitled "#+TITLE: Daily Logs"
      str-targets "*** Targets / Macros"
      hed-targets "| Type | Daily [Min,Max] | Weekly [Min,Max] | Monthly [Min,Max] |"
      str-daylogs "*** Logs"
      hed-daylogs "| Timestamp | Type | Item | Amount | Calories |")
;;      str-summary "*** Summaries")

(defun logbook-makeheaders ()
  "Make table headers."
  (let ((hed-year (format-time-string "* %Y"))
        (hed-month (format-time-string "** %m - %b"))
        (tbl-macros (format-time-string "%Y-%m-Macros"))
        (tbl-logs (format-time-string "%Y-%m-Logbook")))
    (with-current-buffer (find-file-noselect logbookfile)
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
        (end-of-buffer)
        (insert (format "\n%s\n%s\n" hed-year hed-month)))
      ;; Search for headers
      ;; Search for macros table
      (unless (search-forward tbl-macros nil t)
        (database-maketable str-targets tbl-macros hed-targets))
      ;; Search for logs table
      (unless (search-forward tbl-macros nil t)
        (database-maketable str-daylogs tbl-logs hed-daylogs)))))
