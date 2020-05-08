(setq databasefile "~/database.org")


(defvar database nil)

(defun database-sync ()
  "Sync database to database file.")

(defun validate-entry (pentry)
  "Checking the contents of PENTRY."
  (let ((judgement "Keeping inconsistency, but secretly judging you for it.")
        (kc (plist-get pentry :kc))
        (fat (plist-get pentry :fat))
        (carbs (plist-get pentry :carbs))
        (fibre (plist-get pentry :carbs-fibre))
        (sugars (plist-get pentry :carbs-sugars))
        (sodium (plist-get pentry :sodium))
        (portion (plist-get pentry :portion))
        (protein (plist-get pentry :protein)))
    (unless (or (> fibre carbs) (> sugars carbs))
      (let ((newcarbs (+ fibre sugars)))
        (if (y-or-n-p (format "Fibre(%sg) or Sugar(%sg) are > Total Carbs(%sg).\nChange Total Carbs to %sg? " fibre sugars carbs newcarbs))
            (setq carbs newcarbs)
          (message judgement))))
    (let ((newkc (+ (* 4 (- carbs fibre)) (* 4 protein) (* 7 fat))))
      (unless (= newkc kc)
        (if (y-or-n-p (format "(4*((carbs - fibre) + protein)) + (7*fat) =\n(4*((%s - %s) + %s)) + (7*%s) = %s (and not the assigned %s).\nChange total KC for this portion to %s? " carbs fibre protein fat newkc kc newkc))
            (setq kc newkc)
          (message judgement))))
    `(:kc ,kc :portion ,portion
          :carbs ,carbs :carbs-fibre ,fibre :carbs-sugars ,sugars
          :protein ,protein :fat ,fat :sodium ,sodium)))


(defun get-newentry (fname)
  "Create a new plist food entry named FNAME."
  (if (y-or-n-p "Search online? ")
      (online-retrieve fname)
    (let* ((result (read-string
                   (concat "[" fname "]:\
 kc portion carbs ~fibre ~sugars protein fat sodium(mg)\n")))
           (parsed (loop for num in (split-string result)
                         collect (string-to-number num)))
           (plistinp
            `(:kc ,(nth 0 parsed) :portion ,(nth 1 parsed)
                  :carbs ,(nth 2 parsed) :carbs-fibre ,(nth 3 parsed)
                  :carbs-sugars ,(nth 4 parsed) :protein ,(nth 5 parsed)
                  :fat ,(nth 6 parsed) :sodium ,(nth 7 parsed))))
      (validate-entry plistinp))))


(defun database-generate ()
  (unless (file-exists-p databasefile)
    ;; Make table header
    (with-current-buffer (get-buffer-create databasefile)
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Database of Foodstuffs\n")
      (insert "\n")
      (insert "| kCal | Portion(g) | Carbs(g) | ofFibre(g) | ofSugars(g) | Protein(g) | Fat(g) | Sodium (mg) |\n")
      (insert "|-\n")
      (insert "| |")
      (org-table-align)))
  ;; Parse the table
  (goto-line 3)
  (org-table-begin)



(defun database-insert (fname &optional plist-info)
  "Insert food FNAME with PLIST-INFO."
  (cl-pushnew (cons fname
                    (or plist-info (get-newentry fname))
                    (or database (database-generate))))
  (database-sync))


(defun database-retrieve (fname)
  "Retrieve food plist on FNAME from db."
  (alist-get fname (or database (generate-database))))
