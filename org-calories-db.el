;;; org-calories-db.el --- Database management for org-calories -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

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
(setq databasefile "~/database.org")

(setq db-foods nil
      db-recipes nil
      db-exercises nil)

(defconst str-titled "#+TITLE: Database of Foods, Recipes, and Exercises")
(defconst str-dbfood "* Individual Foods")
(defconst hed-dbfood "| Name | Portion(g) | Calories (kC) | Carbs(g) | ofFibre(g) | ofSugars(g) | Protein(g) | Fat(g) | Sodium (mg) |")
(defconst str-dbrecp "* Recipes")
(defconst hed-dbrecp "| Name | Amount | Ingredients (Foods::Portion(g)[,,] |") ;; variable length nested list
(defconst str-dbexer "* Exercises")
(defconst hed-dbexer "| Name | Amount | Unit | Calories (kC) |")

(defsubst org-calories-db--s2n (num pin)
  "String 2 Num.  Extract the NUM index from PIN, zip it in and zip it out."
  (string-to-number (nth num pin)))

(defun org-calories-db--scale-item (type plist-info amount)
  "For item TYPE, scale PLIST-INFO data by AMOUNT."
  (let* ((scalefield (cond ((eq type 'foods) :portion)
                           ((eq type 'recipes) :amount)
                           ((eq type 'exercises) :amount)
                           (t (user-error "Scale type not found"))))
         (scaleamount (plist-get plist-info scalefield))
         (scalefractn (/ (float scaleamount) amount))
         (newplist nil))
    (dolist (var (reverse plist-info) newplist)
      (if (numberp var)
          (push (round (/ (float var) scalefractn)) newplist)
        (push var newplist))))) ;; keywords or strings


(defun org-calories-db--foods2plist (pin)
  "Convert a single entry list of PIN to food plist."
  `(:portion ,(org-calories-db--s2n 0 pin) :kc ,(org-calories-db--s2n 1 pin)
             :carbs ,(org-calories-db--s2n 2 pin) :fibre ,(org-calories-db--s2n 3 pin)
             :sugars ,(org-calories-db--s2n 4 pin) :protein ,(org-calories-db--s2n 5 pin)
             :fat ,(org-calories-db--s2n 6 pin) :sodium ,(org-calories-db--s2n 7 pin)))

(defun org-calories-db--recipes2plist (pin)
  "Convert a single entry list of PIN to recipes plist."
  (let ((recipeamt (string-to-number (car pin)))
        (recipeingrd nil))
    (dolist (ingredients (split-string (cadr pin) ",,"))
      (let* ((portfood (split-string ingredients "::"))
             (food (nth 0 portfood))
             (port (string-to-number (nth 1 portfood))))
        (push (list :food food :portion port) recipeingrd)))
    (list :amount recipeamt :ingredients recipeingrd)))


(defun org-calories-db--exercises2plist (pin)
  "Convert a single entry list of PIN to exercise plist."
  (let ((unit (--> (nth 1 pin)
                   (cond ((string= it "mins") 'mins)
                         ((string= it "lots") 'lots)
                         (t (user-error "'%s' not a valid exercise unit.\
  Please use either 'mins' or 'lots'" it))))))
    `(:amount ,(org-calories-db--s2n 0 pin) :unit ,unit :kc ,(org-calories-db--s2n 2 pin))))

(defun org-calories-db--maketable (section title header)
  "Insert table with SECTION, TITLE, and HEADER."
  (insert (format "\n\n%s\n\n" section))
  (insert (format "#+NAME:%s\n" title))
  (insert header)
  (org-table-insert-hline)
  (forward-char 1)
  (org-table-insert-row -1)
  (end-of-line 1)
  (insert "\n\n"))

(defun org-calories-db--makeheaders ()
  "Make table headers."
  (with-current-buffer (find-file-noselect databasefile)
    (goto-char 0)
    ;; Make Title if not found
    (unless (search-forward str-titled nil t)
      (insert str-titled)
      (insert "\n\n"))
    ;; Make Food, before Recipe
    (if (search-forward str-dbfood nil t)
        (if (search-forward str-dbrecp nil t)
            (progn (beginning-of-line)
                   (forward-line -1))
          (end-of-buffer))
      (org-calories-db--maketable str-dbfood "Foods" hed-dbfood))
    ;; Make Recipe, vor Exercises
    (if (search-forward str-dbrecp nil t)
        (if (search-forward str-dbexer nil t)
            (progn (beginning-of-line)
                   (forward-line -1))
          (end-of-buffer))
      (org-calories-db--maketable str-dbrecp "Recipes" hed-dbrecp))
    ;; Exercises, nach alles
    (if (search-forward str-dbexer nil t)
        (end-of-buffer)
      (org-calories-db--maketable str-dbexer "Exercises" hed-dbexer))))


(defun org-calories-db--generate (&optional type)
  "Generate the database from the file, and limit to TYPE."
  (org-calories-db--makeheaders)
  (with-current-buffer (find-file-noselect databasefile)
    ;; Parse Tables
    (goto-char 0)
    (let ((sstring nil)
          (s2plist nil)
          (dbsymbl nil))
      (cond ((eq type 'foods) (setq sstring str-dbfood
                                    s2plist #'org-calories-db--foods2plist
                                    dbsymbl 'db-foods))
            ((eq type 'recipes) (setq sstring str-dbrecp
                                      s2plist #'org-calories-db--recipes2plist
                                      dbsymbl 'db-recipes))
            ((eq type 'exercises) (setq sstring str-dbexer
                                        s2plist #'org-calories-db--exercises2plist
                                        dbsymbl 'db-exercises))
            (t (user-error "Database type doesn't exist")))
      ;;
      (if (search-forward sstring nil t)
          (if (re-search-forward org-table-line-regexp nil t)
              (dolist (row (cddr (org-table-to-lisp)))
                (let ((nam (car row))
                      (pin (cdr row)))
                  (if (> (length nam) 1)
                      (cl-pushnew (cons nam (funcall s2plist pin))
                                  (symbol-value dbsymbl)
                                  :test #'string= :key #'car)))))))))


;; (defun database-table-to-list (type)
;;   (let* ((tdata (org-table-to-lisp))
;;          (fdata (cddr tdata))
;;          (parser (cond ((eq type 'foods) #'org-calories-db--foods2plist)
;;                        ((eq type 'recipes) #'org-calories-db--recipes2plist)
;;                        (t (user-error "Doesn't exist."))))
;;          (res-alist nil))
;;     (dolist (ldata fdata res-alist)
;;       (let ((fname (car ldata))
;;             (fdata (funcall parser (cdr ldata))))
;;         (pushnew (cons fname fdata) res-alist :key #'car)))))


(defun org-calories-db--kill-table ()
  (forward-line 2) ;; to data line
  ;; erase current table
  (let ((start (line-beginning-position))
        (ended (progn (re-search-forward "^\\($\\||\s+|\\)")
                      (line-end-position))))
    (kill-region start ended)
    (insert "| |")
    (org-table-align)
    (org-table-goto-column 1)))

(defun org-calories-db--trimandsort ()
  "Trim table and sort on name."
  ;; Trim last empty row
  (progn (kill-line 0)(kill-line 1) (insert "\n")(forward-line -2))
  ;; Sort by name
  (org-table-goto-column 1)
  (org-table-sort-lines nil ?a))

(defun org-calories-db--sync (type)
  "Sync db TYPE back to database file."
  (org-calories-db--makeheaders)
  (with-current-buffer (find-file-noselect databasefile)
    ;; Parse Tables
    (save-excursion
      (goto-char 0)
      (cond ((eq type 'foods)
             (if (search-forward str-dbfood nil t)
                 (when (re-search-forward org-table-line-regexp nil t)
                   (unless db-foods
                     (user-error "db-foods not populated, quitting"))
                   (org-calories-db--kill-table)
                   ;; Dump current food database
                   (dolist (entry db-foods)
                     (insert (car entry)) ;; food name
                     (org-table-next-field)
                     (dolist (keyw '(:portion :kc  :carbs :fibre :sugars
                                              :protein :fat :sodium))
                       (insert (format "%s" (plist-get (cdr entry) keyw)))
                       (org-table-next-field)))
                   (org-calories-db--trimandsort))))
            ((eq type 'recipes)
             (if (search-forward str-dbrecp nil t)
                 (when (re-search-forward org-table-line-regexp nil t)
                   (unless db-recipes
                     (user-error "db-recipes not populated, quitting"))
                   (org-calories-db--kill-table)
                   ;; Dump current recipes database
                   (dolist (entry db-recipes)
                     (insert (car entry)) ;; recipe name
                     (org-table-next-field)
                     (insert (format ;; amount
                              "%s" (plist-get (cdr entry) :amount)))
                     (org-table-next-field)
                     (let ((inglist nil))
                       (dolist (ingr (plist-get (cdr entry) :ingredients))
                         (let ((food (plist-get ingr :food))
                               (port (plist-get ingr :portion)))
                           (push (format "%s::%d" food port) inglist)))
                       (insert (format "%s" (string-join inglist ",,")))
                       (org-table-next-field)))
                   (org-calories-db--trimandsort))))
            ((eq type 'exercises)
             (if (search-forward str-dbexer nil t)
                 (when (re-search-forward org-table-line-regexp nil t)
                   (unless db-exercises
                     (user-error "db-exercises not populated, quitting"))
                   (org-calories-db--kill-table)
                   ;; Dump current exercise database
                   (dolist (entry db-exercises)
                     (insert (car entry)) ;; exercise name
                     (org-table-next-field)
                     (let ((dur (plist-get (cdr entry) :amount))
                           (unt (plist-get (cdr entry) :unit))
                           (cal (plist-get (cdr entry) :kc)))
                       (insert (format "%d" dur))
                       (org-table-next-field)
                       (insert (format "%s" unt))
                       (org-table-next-field)
                       (insert (format "%d" cal))
                       (org-table-next-field)))
                   (org-calories-db--trimandsort))))
            (t (user-error "Doesn't exist")))
      (save-buffer)
      (message "synced %s to %s" type databasefile))))

(provide 'org-calories-db)
