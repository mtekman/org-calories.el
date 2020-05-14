(setq databasefile "~/database.org")

(setq db-foods nil)
(setq db-recipes nil)

(defsubst db-s2n (num pin)
  "String 2 Num. Extract the NUM index from PIN, zip it in and zip it out."
  (string-to-number (nth num pin)))

(defun db-foods-2plist (pin)
  "Convert a single entry list of PIN to food plist."
  `(:kc ,(db-s2n 0 pin) :portion ,(db-s2n 1 pin)
        :carbs ,(db-s2n 2 pin) :fibre ,(db-s2n 3 pin) :sugars ,(db-s2n 4 pin)
        :protein ,(db-s2n 5 pin) :fat ,(db-s2n 6 pin) :sodium ,(db-s2n 7 pin)))

(defun db-recipes-2plist (pin)
  "Convert a single entry list of PIN to recipes plist."
  (let ((recipealist nil))
    (dolist (ingredients (split-string pin ",,") recipealist)
      (let* ((portfood (split-string ingredients "::"))
             (food (nth 0 portfood))
             (port (string-to-number (nth 1 portfood))))
        (push (list :food food :portion port) recipealist)))))


(defun database-makeheaders ()
  (let ((str-titled "#+TITLE: Database of Foodstuffs")
        (str-dbfood "* Individual Foods")
        (str-dbrecp "* Recipes"))
    (with-current-buffer (find-file-noselect databasefile)
      (goto-char 0)
      ;; Make Headers
      (unless (search-forward str-titled nil t)
        (insert "\n\n")
        (insert str-titled)
        (insert "\n\n"))
      (if (search-forward str-dbfood nil t)
          (if (search-forward str-dbrecp nil t)
              (progn (beginning-of-line)
                     (forward-line -1))
            (end-of-buffer))
        (insert "\n\n")
        (insert str-dbfood)
        (insert "\n\n")
        (insert "| Name | kCal | Portion(g) | Carbs(g) | ofFibre(g) | ofSugars(g) | Protein(g) | Fat(g) | Sodium (mg) |")
        (org-table-insert-hline)
        (forward-char 1)
        (org-table-insert-row -1)
        (end-of-line 1)
        (insert "\n\n"))
      (if (search-forward str-dbrecp nil t)
          (end-of-buffer)
        (insert "\n\n")
        (insert str-dbrecp)
        (insert "\n\n")
        (insert "| Name | Ingredients (Foods::Portion(g)[,,] |")
        (org-table-insert-hline)
        (forward-char 1)
        (org-table-insert-row -1)
        (end-of-line 1)
        (insert "\n\n")))))


(defun database-generate (&optional type)
  "Generate the database from the file, and limit to TYPE."
  (database-makeheaders)
  (let ((str-titled "#+TITLE: Database of Foodstuffs")
        (str-dbfood "* Individual Foods")
        (str-dbrecp "* Recipes"))
    (with-current-buffer (find-file-noselect databasefile)
      ;; Parse Tables
      (goto-char 0)
      (cond ((eq type 'foods)
             (if (search-forward "* Individual Foods" nil t)
                 (if (re-search-forward org-table-line-regexp nil t)
                     (dolist (row (cddr (org-table-to-lisp)))
                       (let ((nam (car row))
                             (pin (cdr row)))
                         (if (> (length nam) 1)
                             (cl-pushnew (cons nam (db-foods-2plist pin))
                                         db-foods
                                         :test #'string= :key #'car)))))))
            ((eq type 'recipes)
             (if (search-forward "* Recipes" nil t)
                 (if (re-search-forward org-table-line-regexp nil t)
                     (dolist (row (cddr (org-table-to-lisp)))
                       (let ((nam (car row))
                             (pin (cadr row)))
                         (if (> (length nam) 1)
                             (cl-pushnew (cons nam (db-recipes-2plist pin))
                                         db-recipes
                                         :test #'string= :key #'car)))))))
            (t (user-error "Doesn't exist."))))))


(defun database-table-to-list (type)
  (let* ((tdata (org-table-to-lisp))
         (fdata (cddr tdata))
         (parser (cond ((eq type 'foods) #'db-foods-2plist)
                       ((eq type 'recipes) #'db-recipes-2plist)
                       (t (user-error "Doesn't exist."))))
         (res-alist nil))
    (dolist (ldata fdata res-alist)
      (let ((fname (car ldata))
            (fdata (funcall parser (cdr ldata))))
        (pushnew (cons fname fdata) res-alist :key #'car)))))


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


(defun database-sync (type)
  "Sync db-foods to db-foods file."
  (database-makeheaders)
  (let ((str-titled "#+TITLE: Database of Foodstuffs")
        (str-dbfood "* Individual Foods")
        (str-dbrecp "* Recipes"))
    (with-current-buffer (find-file-noselect databasefile)
      ;; Parse Tables
      (save-excursion
        (goto-char 0)
        (cond ((eq type 'foods)
               (if (search-forward "* Individual Foods" nil t)
                   (when (re-search-forward org-table-line-regexp nil t)
                     (database-kill-table)
                     ;; Dump current food database
                     (dolist (entry db-foods)
                       (insert (car entry)) ;; food name
                       (org-table-next-field)
                       (dolist (keyw '(:kc :portion :carbs :fibre :sugars
                                           :protein :fat :sodium))
                         (insert (format "%s" (plist-get (cdr entry) keyw)))
                         (org-table-next-field)))
                     ;; Trim last empty row
                     (progn (kill-line 0)(kill-line 1) (insert "\n")(forward-line -2))
                     ;; Sort by name
                     (org-table-goto-column 1)
                     (org-table-sort-lines nil ?a))))
              ((eq type 'recipes)
               (if (search-forward "* Recipes" nil t)
                   (when (re-search-forward org-table-line-regexp nil t)
                     (database-kill-table)
                     ;; Dump current recipes database
                     (dolist (entry db-recipes)
                       (insert (car entry)) ;; recipe name
                       (org-table-next-field)
                       (let ((inglist nil))
                         (dolist (ingr (cdr entry))
                           (let ((food (plist-get ingr :food))
                                 (port (plist-get ingr :portion)))
                             (push (format "%s::%d" food port) inglist)))
                         (insert (format "%s" (string-join inglist ",,")))
                         (org-table-next-field)))
                     ;; Trim last empty row
                     (progn (kill-line 0)(kill-line 1) (insert "\n")(forward-line -2))
                     ;; Sort by name
                     (org-table-goto-column 1)
                     (org-table-sort-lines nil ?a))))                       
              (t (user-error "Doesn't exist.")))))))


    ;; (forward-line 2)
    ;; (org-table-begin)
    ;; (forward-line 2)
    ;; (dolist (fentry db-foods)
    ;;   (let ((fname (car fentry))
    ;;         (pinfo (cdr fentry)))
    ;;     (org-table-next-field)
    ;;     (let* ((fname-present (substring-no-properties
    ;;                            (string-trim (org-table-get-field))))
    ;;            (fname-exists (and (> (length fname-present) 1)
    ;;                               (alist-get fname-present db-foods))))
    ;;       (if fname-exists
    ;;           (forward-line 1)
    ;;         ;; If this already exists in the db, check values
    ;;         ;; Otherwise insert new
    ;;         (insert fname)
    ;;         (org-table-next-field)
    ;;         (dolist (pper '(:kc :portion :carbs :fibre :sugars
    ;;                             :protein :fat :sodium))
    ;;           (let ((num (plist-get pinfo pper)))
    ;;             (insert (format "%s" num))
    ;;             (org-table-next-field)))))))
    ;; (org-table-next-field)
    ;; (org-table-sort-lines)))


(provide 'org-calories-database)
