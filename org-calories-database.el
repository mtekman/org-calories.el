
;;; Code:
(setq databasefile "~/database.org")

(setq db-foods nil
      db-recipes nil
      db-exercises nil)

(setq str-titled "#+TITLE: Database of Foods, Recipes, and Exercises"
      str-dbfood "* Individual Foods"
      hed-dbfood "| Name | Portion(g) | Calories (kC) | Carbs(g) | ofFibre(g) | ofSugars(g) | Protein(g) | Fat(g) | Sodium (mg) |"
      str-dbrecp "* Recipes"
      hed-dbrecp "| Name | Amount (units) | Ingredients (Foods::Portion(g)[,,] |" ;; variable length nested list
      str-dbexer "* Exercises"
      hed-dbexer "| Name | Duration (mins) | Calories (kC) |")

(defsubst db-s2n (num pin)
  "String 2 Num.  Extract the NUM index from PIN, zip it in and zip it out."
  (string-to-number (nth num pin)))

(defun db-scale-item (type plist-info amount)
  "For item TYPE, scale PLIST-INFO data by AMOUNT."
  (let* ((scalefield (cond ((eq type 'foods) :portion)
                           ((eq type 'recipes) :amount) ;;-- not used
                           ((eq type 'exercises) :duration)
                           (t (user-error "Scale type not found"))))
         (scaleamount (plist-get plist-info scalefield))
         (scalefractn (/ (float scaleamount) amount))
         (newplist nil))
    (dolist (var (reverse plist-info) newplist)
      (if (keywordp var)
          (push var newplist)
        (push (round (/ (float var) scalefractn)) newplist)))))


(defun db-foods-2plist (pin)
  "Convert a single entry list of PIN to food plist."
  `(:portion ,(db-s2n 0 pin) :kc ,(db-s2n 1 pin)
             :carbs ,(db-s2n 2 pin) :fibre ,(db-s2n 3 pin) :sugars ,(db-s2n 4 pin)
             :protein ,(db-s2n 5 pin) :fat ,(db-s2n 6 pin) :sodium ,(db-s2n 7 pin)))

(defun db-recipes-2plist (pin)
  "Convert a single entry list of PIN to recipes plist."
  (let ((recipeamt (string-to-number (car pin)))
        (recipeingrd nil))
    (dolist (ingredients (split-string (cadr pin) ",,"))
      (let* ((portfood (split-string ingredients "::"))
             (food (nth 0 portfood))
             (port (string-to-number (nth 1 portfood))))
        (push (list :food food :portion port) recipeingrd)))
    (list :amount recipeamt :ingredients recipeingrd)))


(defun db-exercises-2plist (pin)
  "Convert a single entry list of PIN to exercise plist."
  `(:duration ,(db-s2n 0 pin) :kc ,(db-s2n 1 pin)))

(defun database-maketable (section title header)
  "Insert table with SECTION, TITLE, and HEADER."
  (insert (format "\n\n%s\n\n" section))
  (insert (format "#+NAME:%s\n" title))
  (insert header)
  (org-table-insert-hline)
  (forward-char 1)
  (org-table-insert-row -1)
  (end-of-line 1)
  (insert "\n\n"))

(defun database-makeheaders ()
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
      (database-maketable str-dbfood "Foods" hed-dbfood))
    ;; Make Recipe, vor Exercises
    (if (search-forward str-dbrecp nil t)
        (if (search-forward str-dbexer nil t)
            (progn (beginning-of-line)
                   (forward-line -1))
          (end-of-buffer))
      (database-maketable str-dbrecp "Recipes" hed-dbrecp))
    ;; Exercises, nach alles
    (if (search-forward str-dbexer nil t)
        (end-of-buffer)
      (database-maketable str-dbexer "Exercises" hed-dbexer))))


(defun database-generate (&optional type)
  "Generate the database from the file, and limit to TYPE."
  (database-makeheaders)
  (with-current-buffer (find-file-noselect databasefile)
    ;; Parse Tables
    (goto-char 0)
    (let ((sstring nil)
          (s2plist nil)
          (dbsymbl nil))
      (cond ((eq type 'foods) (setq sstring str-dbfood
                                    s2plist #'db-foods-2plist
                                    dbsymbl 'db-foods))
            ((eq type 'recipes) (setq sstring str-dbrecp
                                      s2plist #'db-recipes-2plist
                                      dbsymbl 'db-recipes))
            ((eq type 'exercises) (setq sstring str-dbexer
                                        s2plist #'db-exercises-2plist
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
;;          (parser (cond ((eq type 'foods) #'db-foods-2plist)
;;                        ((eq type 'recipes) #'db-recipes-2plist)
;;                        (t (user-error "Doesn't exist."))))
;;          (res-alist nil))
;;     (dolist (ldata fdata res-alist)
;;       (let ((fname (car ldata))
;;             (fdata (funcall parser (cdr ldata))))
;;         (pushnew (cons fname fdata) res-alist :key #'car)))))


(defun database-kill-table ()
  (forward-line 2) ;; to data line
  ;; erase current table
  (let ((start (line-beginning-position))
        (ended (progn (re-search-forward "^\\($\\||\s+|\\)")
                      (line-end-position))))
    (kill-region start ended)
    (insert "| |")
    (org-table-align)
    (org-table-goto-column 1)))

(defun database-trimandsort ()
  "Trim table and sort on name."
  ;; Trim last empty row
  (progn (kill-line 0)(kill-line 1) (insert "\n")(forward-line -2))
  ;; Sort by name
  (org-table-goto-column 1)
  (org-table-sort-lines nil ?a))

(defun database-sync (type)
  "Sync db-foods to db-foods file."
  (database-makeheaders)
  (with-current-buffer (find-file-noselect databasefile)
    ;; Parse Tables
    (save-excursion
      (goto-char 0)
      (cond ((eq type 'foods)
             (if (search-forward str-dbfood nil t)
                 (when (re-search-forward org-table-line-regexp nil t)
                   (unless db-foods
                     (user-error "db-foods not populated, quitting"))
                   (database-kill-table)
                   ;; Dump current food database
                   (dolist (entry db-foods)
                     (insert (car entry)) ;; food name
                     (org-table-next-field)
                     (dolist (keyw '(:portion :kc  :carbs :fibre :sugars
                                              :protein :fat :sodium))
                       (insert (format "%s" (plist-get (cdr entry) keyw)))
                       (org-table-next-field)))
                   (database-trimandsort))))
            ((eq type 'recipes)
             (if (search-forward str-dbrecp nil t)
                 (when (re-search-forward org-table-line-regexp nil t)
                   (unless db-recipes
                     (user-error "db-recipes not populated, quitting"))
                   (database-kill-table)
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
                   (database-trimandsort))))
            ((eq type 'exercises)
             (if (search-forward str-dbexer nil t)
                 (when (re-search-forward org-table-line-regexp nil t)
                   (unless db-exercises
                     (user-error "db-exercises not populated, quitting"))
                   (database-kill-table)
                   ;; Dump current exercise database
                   (dolist (entry db-exercises)
                     (insert (car entry)) ;; exercise name
                     (org-table-next-field)
                     (let ((dur (plist-get (cdr entry) :duration))
                           (cal (plist-get (cdr entry) :kc)))
                       (insert (format "%d" dur))
                       (org-table-next-field)
                       (insert (format "%d" cal))
                       (org-table-next-field)))
                   (database-trimandsort))))
            (t (user-error "Doesn't exist")))
      (save-buffer)
      (message "synced %s to %s" type databasefile))))

(provide 'org-calories-database)
