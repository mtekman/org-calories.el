(require 'org-calories-database)

;;; Code:
(defun db-foods-validateentry (pentry)
  "Checking the contents of PENTRY."
  (let ((judgement "Keeping inconsistency, but secretly judging you for it.")
        (kc (plist-get pentry :kc)) (portion (plist-get pentry :portion))
        (carbs (plist-get pentry :carbs)) (fibre (plist-get pentry :fibre)) (sugars (plist-get pentry :sugars))
        (protein (plist-get pentry :protein)) (fat (plist-get pentry :fat)) (sodium (plist-get pentry :sodium)))
    (let ((visiblecarbs (+ fibre sugars)))
      (unless (<= visiblecarbs carbs)
        (if (y-or-n-p
             (format
              "Fibre(%sg) + Sugar(%sg) are > Total Carbs(%sg).  Change Total Carbs to %sg?\n\
(Note: Fibre contributes towards Total Carbs, but is subtracted when calculating *Calories* from Carbs)"
              fibre sugars carbs visiblecarbs))
            (setq carbs visiblecarbs)
          (message judgement))))
    (let ((newkc (+ (* 4 (- carbs fibre)) (* 4 protein) (* 7 fat))))
      (unless (= newkc kc)
        (if (y-or-n-p
             (format
              "[Calculation Error]\n\
  (4*((carbs - fibre) + protein)) + (7*fat) = (4*((%s - %s) + %s)) + (7*%s) = %s kCal\n\
This is not equal to the assigned %s kCal.  Set Calories for this portion to  %s kCal instead? "
              carbs fibre protein fat newkc kc newkc))
            (setq kc newkc)
          (message judgement))))
    `(:portion ,portion :kc ,kc
          :carbs ,carbs :fibre ,fibre :sugars ,sugars
          :protein ,protein :fat ,fat :sodium ,sodium)))

(defun db-foods-newentry (fname)
  "Create a new plist food entry named FNAME."
  ;;(if (y-or-n-p "Search online? ") (online-retrieve fname)
  (let* ((result (read-string
                  (concat "[" fname "] -- kCals and Grams:\n\
kc\tportion\tcarbs\t~fibre\t~sugars\tprotein\tfat\tsodium(mg)\n")))
         (plistinp (db-foods-2plist (split-string result))))
    (db-foods-validateentry plistinp)))

(defun db-foods-insert (fname &optional plist-info)
  "Insert food FNAME with PLIST-INFO."
  (interactive "sFood Name: ")
  (if (db-foods-retrieve fname)
      (message "Food '%s' already exists in the food table, not inserting." fname)
    (cl-pushnew
     (cons fname (or plist-info (db-foods-newentry fname)))
     db-foods :key #'car)
    (database-sync 'foods)))

(defun db-foods-retrieve (fname)
  "Retrieve food plist on FNAME from db."
  (database-generate 'foods)
  (alist-get fname db-foods nil nil #'string-equal))
;;
;;
(defun db-recipes-newentry (rname)
  "Create a new plist recipe entry named RNAME."
  (let* ((result
          (read-string
           (concat "[" rname "] -- amount, then pairs of\
 food::portion(g)[,,food::portion(g)] ingredient items:\n"))))
    (db-recipes-2plist (split-string result))))

(defun db-recipes-insert (rname &optional plist-info)
  "Insert recipes RNAME with PLIST-INFO (an array of food and portions)."
  (interactive "sRecipe Name: ")
  (if (db-recipes-retrieve rname)
      (message "Recipe '%s' already exists in the recipes table, not inserting." rname)
    (cl-pushnew
     (cons rname (or plist-info (db-recipes-newentry rname)))
     db-recipes :key #'car)
    (database-sync 'recipes)))

(defun db-recipes-retrieve (rname)
  "Retrieve recipe plist on RNAME from db.  No need to sync."
  (database-generate 'recipes)
  (alist-get rname db-recipes nil nil #'string-equal))
;;
;;
(defun db-exercises-newentry (ename)
  "Create a new plist exercise entry named ENAME."
  (let* ((result
          (read-string (concat "[" ename "] -- Duration(Mins) and Calories(kC): "))))
    (db-exercises-2plist (split-string result))))

(defun db-exercises-insert (ename &optional plist-info)
  "Define exercise ENAME with PLIST-INFO of description, duration, and calories."
  (interactive "sExercise Name: ")
  (if (db-exercises-retrieve ename)
      (message "Exercise '%s' already exists in the exercises table, not inserting." ename)
    (cl-pushnew
     (cons ename (or plist-info (db-exercises-newentry ename)))
     db-exercises :key #'car)
    (database-sync 'exercises)))

(defun db-exercises-retrieve (rname)
  "Retrieve exercise RNAME from db."
  (database-generate 'exercises)
  (alist-get rname db-exercises nil nil #'string-equal))


;;;; -{Tests}-
;; (db-foods-insert "fruit1" '(:kc 110 :portion 100 :carbs 50 :fibre 30 :sugars 10 :protein 10 :fat 5 :sodium 123))
;; (db-foods-insert "fruit2" '(:kc 120 :portion 100 :carbs 50 :fibre 30 :sugars 10 :protein 10 :fat 5 :sodium 123))
;; (db-foods-insert "fruit3" '(:kc 130 :portion 100 :carbs 50 :fibre 30 :sugars 10 :protein 10 :fat 5 :sodium 123))
;; (db-foods-retrieve "fruit2")
;; (db-foods-insert "chew")
;; (db-recipes-insert "fruit salad" '((:food "fruit1" :portion 30) (:food "fruit2" :portion 120) (:food "fruit3" :portion 50)))
;; (db-recipes-retrieve "fruit salad")
(provide 'org-calories-food)
;;; org-calories-foods.el ends here
