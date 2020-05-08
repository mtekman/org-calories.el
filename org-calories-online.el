

(defun online-search (fname))

(defun online-parselist (results)
  "Parse web RESULTS into plists."
  (loop for res in results
        collect (online-parseresult res)))

(defcustom userformat-string "something"
  "Carb Fibre Sugars Fat Protein Sodium."
  :type 'string
  :group 'org-calories)

(defun online-userlist (plist)
  "Convert food PLIST into a description."
  (loop for pl in plist
        collect (format userformat-string plist)))

(defun online-retrieve (fname)
  "Search for FNAME in backend database"
  (let* ((results (online-search fname))
         (resplist (online-parselist results))
         (resoffer (online-userlist resplist)))
    (if resplist
        (read-answer (concat fname ": ") resoffer)
      (message "No results found."))))
