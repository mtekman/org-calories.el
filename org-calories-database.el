(setq databasefile "~/database.org")

(setq db-foods nil)
(setq db-recipes nil)

(defsubst db-s2n (num pin)
  "String 2 Num. Extract the NUM index from PIN, zip it in and zip it out."
  (string-to-number (nth num pin)))

(defun db-foods-2plist (pin)
  "Resident PIN ."
  `(:kc ,(db-s2n 0 pin) :portion ,(db-s2n 1 pin)
        :carbs ,(db-s2n 2 pin) :fibre ,(db-s2n 3 pin) :sugars ,(db-s2n 4 pin)
        :protein ,(db-s2n 5 pin) :fat ,(db-s2n 6 pin) :sodium ,(db-s2n 7 pin)))

(defun db-recipes-2plist (pin)
  "Resident PIN."
  (let ((recipealist nil))
    (dolist (ingredients (split-string pin ",,"))
      (dolist (portfood (split-string pin "::"))
        (let ((port (nth 0 portfood))
              (food (nth 1 portfood)))
          (push (cons port food) recipealist))))
    recipealist))


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
        (insert "| Name | Ingredients (Portion(g)::Foods,,) |")
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
                             (push (cons nam (db-foods-2plist pin))
                                   db-foods)))))))
            ((eq type 'recipes)
             (if (search-forward "* Recipes" nil t)
                 (if (re-search-forward org-table-line-regexp nil t)
                     (dolist (row (cddr (org-table-to-lisp)))
                       (let ((nam (car row))
                             (pin (cdr row)))
                         (if (> (length nam) 1)
                             (push (cons nam (db-recipes-2plist pin))
                                   db-recipes)))))))
            (t (user-error "Doesn't exist."))))))


(defun database-sync ()
  "Sync db-foods to db-foods file."
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
                     ;;
                     ())))
            ((eq type 'recipes)
             (if (search-forward "* Recipes" nil t)
                 (if (re-search-forward org-table-line-regexp nil t)
                     ;;
                     ())))
            (t (user-error "Doesn't exist."))))))

  
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
