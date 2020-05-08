





(setq logfile "~/logs.org")

(defun get-fromdatabase (fname)
  "Get food FNAME from database, or insert it if not present."
  (or (database-retrieve fname) (database-insert fname)))


(defun get-calorie-info (fname)
  (let ((entry (get-fromdatabase fname)))))
