;;; Code:
(require 'org-calories-logbook)

(defun macros-get (&optional year month)
  "Get daily target macros for YEAR and MONTH."
  (logbook-makeheaders)
  (let* ((marcs nil)
         (myear (or year (string-to-number
                          (format-time-string "%Y"))))
         (mmont (or month (string-to-number
                           (format-time-string "%m"))))
         (tblym (format "#+NAME:%4d-%02d-Macros" myear mmont)))
    (with-current-buffer (find-file-noselect logbookfile)
      (goto-char 0)
      (unless (search-forward tblym nil t)
        (user-error "Macros table %4d-%02d-Macros not available."
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

(defun totals-get (&optional year month day)
  "Get daily total macros for YEAR and MONTH, up to current DAY."
  (logbook-makeheaders)
  (let* ((myear (or year (string-to-number
                          (format-time-string "%Y"))))
         (mmont (or month (string-to-number
                           (format-time-string "%m"))))
         (tblym (format "#+NAME:%4d-%02d-Logbook" myear mmont)))
    (with-current-buffer (find-file-noselect logbookfile)
      (goto-char 0)
      (unless (search-forward tblym nil t)
        (user-error "Macros table %4d-%02d-Logbook not available."
                    myear mmont))
      (forward-line 1)
      (let ((per-diem (make-hash-table :test 'equal)))
        (dolist (line (cddr (org-table-to-lisp)))
          (let ((timestamp (nth 0 line))
                (type (intern (downcase (nth 1 line))))
                (item (nth 2 line))
                (amount (string-to-number (nth 3 line)))
                (kc (string-to-number (nth 4 line))))
            (let* ((timedata (cadr (org-timestamp-from-string timestamp)))
                   (timekey (format "%4d-%02d-%02d"
                                    (plist-get timedata :year-start)
                                    (plist-get timedata :month-start)
                                    (plist-get timedata :day-start)))
                   ;; TODO: db-scale-item fails for recipes... we should
                   ;;       change the fn so that the food recursive parts
                   ;;       are performed inside the fn instead of outside
                   (food-current (db-scale-item type
                                                (db-foods-retrieve item)
                                                amount)))
              (if (not (gethash timekey per-diem))
                  (puthash timekey food-current per-diem)
                (let* ((food-hashed (gethash timekey per-diem))
                       (food-total (db-foods-add food-hashed food-current)))
                  (puthash timekey food-total per-diem))))))
        per-diem))))
                     
