(require 'org-calories-database)

(defun db-foods-validateentry (pentry)
  "Checking the contents of PENTRY."
  (let ((judgement "Keeping inconsistency, but secretly judging you for it.")
        (kc (plist-get pentry :kc)) (portion (plist-get pentry :portion))
        (carbs (plist-get pentry :carbs)) (fibre (plist-get pentry :fibre)) (sugars (plist-get pentry :sugars))
        (protein (plist-get pentry :protein)) (fat (plist-get pentry :fat)) (sodium (plist-get pentry :sodium)))
    (unless (or (> fibre carbs) (> sugars carbs))
      (let ((newcarbs (+ fibre sugars)))
        (if (y-or-n-p (format "Fibre(%sg) or Sugar(%sg) are > Total Carbs(%sg).\nChange Total Carbs to %sg? "
                              fibre sugars carbs newcarbs))
            (setq carbs newcarbs)
          (message judgement))))
    (let ((newkc (+ (* 4 (- carbs fibre)) (* 4 protein) (* 7 fat))))
      (unless (= newkc kc)
        (if (y-or-n-p (format "(4*((carbs - fibre) + protein)) + (7*fat) =\n\
(4*((%s - %s) + %s)) + (7*%s) = %s (and not the assigned %s).\n\
Change total KC for this portion to %s? " carbs fibre protein fat newkc kc newkc))
            (setq kc newkc)
          (message judgement))))
    `(:kc ,kc :portion ,portion
          :carbs ,carbs :fibre ,fibre :sugars ,sugars
          :protein ,protein :fat ,fat :sodium ,sodium)))


(defun db-foods-newentry (fname)
  "Create a new plist food entry named FNAME."
  (if (y-or-n-p "Search online? ")
      (online-retrieve fname)
    (let* ((result (read-string
                   (concat "[" fname "]:\
 kc portion carbs ~fibre ~sugars protein fat sodium(mg)\n")))
           (parsed (loop for num in (split-string result)
                         collect (string-to-number num)))
           (plistinp (db-foods-2plist parsed)))
      (db-foods-validateentry plistinp))))


(defun db-foods-insert (fname &optional plist-info)
  "Insert food FNAME with PLIST-INFO."
  (database-generate 'foods)
  (cl-pushnew
   (cons fname (or plist-info (db-foods-newentry fname)))
   db-foods)
  (database-sync 'foods))


(defun db-foods-retrieve (fname)
  "Retrieve food plist on FNAME from db."
  (database-generate 'foods)
  (alist-get fname db-foods)
  (database-sync 'foods))


(db-foods-insert "fruit1" '(:kc 110 :portion 100 :carbs 50 :fibre 30 :sugars 10 :protein 10 :fat 5 :sodium 123))
(db-foods-insert "fruit2" '(:kc 120 :portion 100 :carbs 50 :fibre 30 :sugars 10 :protein 10 :fat 5 :sodium 123))
(db-foods-insert "fruit3" '(:kc 130 :portion 100 :carbs 50 :fibre 30 :sugars 10 :protein 10 :fat 5 :sodium 123))
