;;; org-calories-entry.el --- Different database entry types for org-calories -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-calories.el
;; Keywords: outlines
;; Package-Requires: ((emacs "26.1") (dash "2.17.0") (org "9.1.6"))
;; Version: 0.1

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
(require 'org-calories-db)

(defun org-calories-entry--foods-validateentry (pentry)
  "Checking the contents of PENTRY."
  (let ((judgement "Keeping inconsistency, but secretly judging you for it.")
        (kc (plist-get pentry :kc)) (amount (plist-get pentry :amount)) (unit (plist-get pentry :unit))
        (carbs (plist-get pentry :carbs)) (fibre (plist-get pentry :fibre)) (sugars (plist-get pentry :sugars))
        (protein (plist-get pentry :protein)) (fat (plist-get pentry :fat)) (sodium (plist-get pentry :sodium)))
    (let ((visiblecarbs (+ fibre sugars)))
      (unless (<= visiblecarbs carbs)
        (if (y-or-n-p
             (format
              "(Note: Fibre contributes towards Total Carbs, but is subtracted when calculating *Calories* from Carbs)\n\
Fibre(%sg) + Sugar(%sg) are > Total Carbs(%sg).  Change Total Carbs to %sg? "
              fibre sugars carbs
              (format (if (floatp visiblecarbs) "%.1f" "%d") visiblecarbs)))
            (setq carbs visiblecarbs)
          (message judgement))))
    (let ((newkc (+ (* 4 (- carbs fibre)) (* 4 protein) (* 9 fat))))
      (unless (= newkc kc)
        (if (y-or-n-p
             (format
              "[Calculation Error]\n\
 (4*((carbs - fibre) + protein)) + (9*fat) =\n\
 (4*((%s - %s) + %s)) + (7*%s) = %s kCal\n\
This is not equal to the assigned %s kCal.  Set Calories for this portion to  %s kCal instead? "
              (format (if (floatp carbs) "%.1f" "%d") carbs)
              fibre protein fat newkc kc newkc))
            (setq kc newkc)
          (message judgement))))
    `(:amount ,amount :unit ,unit :kc ,kc
              :carbs ,carbs :fibre ,fibre :sugars ,sugars
              :protein ,protein :fat ,fat :sodium ,sodium)))

(defun org-calories-entry--foods-newentry (fname)
  "Create a new plist food entry named FNAME."
  (let* ((result (read-string
                  (concat "[" fname "] -- kCals and Grams:\n\
Amnt\tUnit\tkcal\tcarbs\t~fibre\t~sugars\tprotein\tfat\tsodium(mg)\n")))
         (plistinp (org-calories-db--foods2plist (split-string result))))
    (org-calories-entry--foods-validateentry plistinp)))

(defun org-calories-entry-foods-insert (fname &optional plist-info)
  "Insert food FNAME with PLIST-INFO."
  (interactive "sFood Name: ")
  (if (org-calories-entry--foods-retrieve fname)
      (message "Food '%s' already exists in the food table, not inserting." fname)
    (unless plist-info
      (let* ((insmeth (read-multiple-choice "Insert method: "
                                            '((?o "Online Search")
                                              (?m "Manual Input"))))
             (chosen (car insmeth)))
        (cond ((eq chosen ?o)
               (--> (org-calories-online-search fname)
                    (let* ((onlname (plist-get it :food))
                           (newname (read-string "Food name: " onlname)))
                      (setq fname newname
                            plist-info (plist-get it :food-info)))))
              ((eq chosen ?m)
               (setq plist-info (org-calories-entry--foods-newentry fname)))
              (t (user-error "Invalid selection")))))
    ;;
    (cl-pushnew (cons fname plist-info)
                org-calories-db--foods :key #'car)
    (org-calories-db--sync 'foods))
  ;; echo out the details in case they were updated
  (cons fname plist-info))

(defun org-calories-entry--foods-retrieve (fname)
  "Retrieve food plist on FNAME from db."
  (org-calories-db--generate 'foods)
  (alist-get fname org-calories-db--foods nil nil #'string-equal))
;;
(defun org-calories-entry--recipes-newentry (rname)
  "Create a new plist recipe entry named RNAME."
  (let* ((inpstr (split-string
                  (read-string
                   (concat "[" rname "] -- Recipe Amount, then pairs of \
Food::Amount  [Food::Amount] ingredient items:\n\
RecipeAmnt\t\tFood::Amount\t\tFood::Amount\t\tetc.\n"))))
         (amount (car inpstr))
         (rest (string-join (cdr inpstr) ",,")))
    (org-calories-db--recipes2plist (list amount rest))))


(defun org-calories-entry-recipes-insert (rname &optional plist-info)
  "Insert recipes RNAME with PLIST-INFO (an array of food and portions)."
  (interactive "sRecipe Name: ")
  (if (org-calories-entry--recipes-retrieve rname)
      (message "Recipe '%s' already exists in the recipes table, not inserting." rname)
    (cl-pushnew
     (cons rname (or plist-info (org-calories-entry--recipes-newentry rname)))
     org-calories-db--recipes :key #'car)
    (org-calories-db--sync 'recipes)))

(defun org-calories-entry--recipes-retrieve (rname)
  "Retrieve recipe plist on RNAME from db.  No need to sync."
  (org-calories-db--generate 'recipes)
  (alist-get rname org-calories-db--recipes nil nil #'string-equal))

(defun org-calories-entry--foods-add (finfo1 finfo2)
  "Add food info FINFO1 and FINFO2.  Foods should be scaled first."
  (let ((adder (lambda (f1 f2 kw)(+ (plist-get f1 kw) (plist-get f2 kw)))))
    (if (and finfo1 finfo2)
        (list :kc (funcall adder finfo1 finfo2 :kc)
              :carbs (funcall adder finfo1 finfo2 :carbs)
              :fibre (funcall adder finfo1 finfo2 :fibre)
              :sugars (funcall adder finfo1 finfo2 :sugars)
              :protein (funcall adder finfo1 finfo2 :protein)
              :fat (funcall adder finfo1 finfo2 :fat)
              :sodium (funcall adder finfo1 finfo2 :sodium))
      ;; return one or the other
      (or finfo1 finfo2))))
;;
(defun org-calories-entry--recipes-calculate (recipe-info)
  "Calculate the food content of the ingredients given by RECIPE-INFO."
  (let ((amount-native (plist-get recipe-info :amount))
        (food-total nil))
    (dolist (var (plist-get recipe-info :ingredients))
      (let* ((foodname (plist-get var :food))
             (fportion (plist-get var :amount))
             (foodinfo (org-calories-entry--foods-retrieve foodname))
             (foodscal (org-calories-db--scale-item 'foods foodinfo fportion)))
        (setq food-total (org-calories-entry--foods-add food-total foodscal))))
    ;; here we add a new field to make it recipe compliant
    (setq food-total (plist-put food-total :amount amount-native))
    food-total))

(defun org-calories-entry--exercises-newentry (ename)
  "Create a new plist exercise entry named ENAME."
  (let* ((result
          (read-string (concat "[" ename "] -- Duration(Mins) and Calories(kC): "))))
    (org-calories-db--exercises2plist (split-string result))))

(defun org-calories-entry-exercises-insert (ename &optional plist-info)
  "Define exercise ENAME with PLIST-INFO of description, duration, and calories."
  (interactive "sExercise Name: ")
  (if (org-calories-entry--exercises-retrieve ename)
      (message "Exercise '%s' already exists in the exercises table, not inserting." ename)
    (cl-pushnew
     (cons ename (or plist-info (org-calories-entry--exercises-newentry ename)))
     org-calories-db--exercises :key #'car)
    (org-calories-db--sync 'exercises)))

(defun org-calories-entry--exercises-retrieve (rname)
  "Retrieve exercise RNAME from db."
  (org-calories-db--generate 'exercises)
  (alist-get rname org-calories-db--exercises nil nil #'string-equal))


;;;; -{Tests}-
;; (org-calories-entry-foods-insert "fruit1" '(:kc 110 :amount 100 :unit 'grams :carbs 50 :fibre 30 :sugars 10 :protein 10 :fat 5 :sodium 123))
;; (org-calories-entry-foods-insert "fruit2" '(:kc 120 :amount 100 :unit 'grams :carbs 50 :fibre 30 :sugars 10 :protein 10 :fat 5 :sodium 123))
;; (org-calories-entry-foods-insert "fruit3" '(:kc 130 :amount 100 :unit 'grams :carbs 50 :fibre 30 :sugars 10 :protein 10 :fat 5 :sodium 123))
;; (org-calories-entry-foods-retrieve "fruit2")
;; (org-calories-entry-foods-insert "chew")
;; (org-calories-entry-recipes-insert "fruit salad" '((:food "fruit1" :amount 30) (:food "fruit2" :amount 120) (:food "fruit3" :amount 50)))
;; (org-calories-entry--recipes-retrieve "fruit salad")

(provide 'org-calories-entry)
;;; org-calories-entry.el ends here
