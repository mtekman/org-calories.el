;;; org-calories-db.el --- Database management for org-calories -*- lexical-binding: t; -*-

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
(require 'org-table)
(require 'subr-x)
(require 'dash)
(require 'map)

(defcustom org-calories-db-file nil
  "Location of database file."
  :group 'org-calories
  :type 'string)

(defvar org-calories-db--foods nil)
(defvar org-calories-db--recipes nil)
(defvar org-calories-db--exercises nil)

(defconst org-calories-db--str-titled "#+TITLE: Database of Foods, Recipes, and Exercises")
(defconst org-calories-db--str-dbfood "* Foods")
(defconst org-calories-db--str-dbrecp "* Recipes")
(defconst org-calories-db--str-dbexer "* Exercises")

(defsubst org-calories-db--s2n (num pin)
  "String 2 Num.  Extract the NUM index from PIN, zip it in and zip it out."
  (string-to-number (nth num pin)))

(defun org-calories-db-show ()
  "Show the database file."
  (interactive)
  (find-file org-calories-db-file))

(defun org-calories-db--scale-item (plist-info amount)
  "For item TYPE, scale PLIST-INFO data by AMOUNT."
  (let* ((scaleamount (plist-get plist-info :amount))
         (scalefractn (/ (float scaleamount) amount))
         (newplist nil))
    (dolist (var (reverse plist-info) newplist)
      (if (numberp var)
          (push (round (/ (float var) scalefractn)) newplist)
        ;; keywords or strings
        (push var newplist)))))


(defun org-calories-db--recipes2plist (pin)
  "Convert a single entry list of PIN to recipes plist."
  (let ((recipeamt (plist-get pin :amount))
        (recipeingrd nil))
    (dolist (ingredients (split-string (plist-get pin :ingredients) ",,"))
      (let* ((portfood (split-string ingredients "::"))
             (food (nth 0 portfood))
             (port (string-to-number (nth 1 portfood))))
        (push (list :food food :amount port) recipeingrd)))
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
  (if section
      (insert (format "\n\n%s\n\n" section)))
  (org-cycle)
  (insert (format "#+NAME:%s\n" title))
  (org-cycle)
  (insert (format "%s" header))
  (org-table-insert-hline)
  (forward-char 1)
  (org-table-insert-row -1)
  (end-of-line 1)
  (insert "\n\n"))

(defun org-calories-db--makeheaders ()
  "Make table headers."
  (with-current-buffer (find-file-noselect org-calories-db-file)
    (goto-char 0)
    ;; Make Title if not found
    (unless (search-forward org-calories-db--str-titled nil t)
      (insert org-calories-db--str-titled)
      (insert "\n\n"))
    ;; Make Food, before Recipe
    (if (search-forward org-calories-db--str-dbfood nil t)
        (if (search-forward org-calories-db--str-dbrecp nil t)
            (progn (beginning-of-line)
                   (forward-line -1))
          (goto-char (point-max)))
      (org-calories-db--maketable org-calories-db--str-dbfood "Food"
                                  "| :name | :amount | :unit | :kc | :fat | :sat | :carbs | :sugars | :fibre | :protein | :salt |"))
    ;; Make Recipe, vor Exercises
    (if (search-forward org-calories-db--str-dbrecp nil t)
        (if (search-forward org-calories-db--str-dbexer nil t)
            (progn (beginning-of-line)
                   (forward-line -1))
          (goto-char (point-max)))
      (org-calories-db--maketable org-calories-db--str-dbrecp "Recipe"
                                  "| :name | :amount | :ingredients | Notes |"))
    ;; Exercises, nach alles
    (if (search-forward org-calories-db--str-dbexer nil t)
        (goto-char (point-max))
      (org-calories-db--maketable org-calories-db--str-dbexer "Exercise"
                                  "| :name | :amount | :unit | :kc |"))))

(defun org-calories-db--parsetypes (unrefinedlist)
  "Change UNREFINEDLIST of strings into keywords and numbers where possible."
  (-map (lambda (str)
          (if (keywordp str) str
            (if (string-prefix-p ":" str) (intern str)
              ;; string to number if possible
              (let ((nval (string-to-number str)))
                ;; 0 is the default convert value
                (if (eq nval 0)
                    (if (string= "0" str) 0 str)
                  nval)))))
        unrefinedlist))

(defun org-calories-db--pairtypes (headers data)
  "Pair keyword HEADERS with DATA."
  (org-calories-db--parsetypes
   (--reduce-from (append acc it) nil
                  (--zip-with (list it other) headers data))))


(defun org-calories-db--generate (&optional type)
  "Generate the database from the file, and limit to TYPE."
  (org-calories-db--makeheaders)
  (with-current-buffer (find-file-noselect org-calories-db-file)
    ;; Parse Tables
    (goto-char 0)
    (let ((sstring nil)
          (dbsymbl nil))
      (cond ((eq type 'foods) (setq sstring org-calories-db--str-dbfood
                                    dbsymbl 'org-calories-db--foods
                                    org-calories-db--foods nil))
            ((eq type 'recipes) (setq sstring org-calories-db--str-dbrecp
                                      dbsymbl 'org-calories-db--recipes
                                      org-calories-db--recipes nil))
            ((eq type 'exercises) (setq sstring org-calories-db--str-dbexer
                                        dbsymbl 'org-calories-db--exercises
                                        org-calories-db--exercises nil))
            (t (user-error "Database type doesn't exist")))
      ;;
      (if (search-forward sstring nil t)
          (if (re-search-forward org-table-line-regexp nil t)
              (let* ((table-data (org-table-to-lisp))
                     (header-order (--map (if (string-prefix-p ":" it) (intern it))
                                          (car (org-table-to-lisp)))))
                (dolist (row (cddr table-data))
                  (let* ((paired-data (org-calories-db--parsetypes
                                       (--reduce-from (append acc it) nil
                                                      (--filter (car it) ;; discard non-keyword columns
                                                                (--zip-with (list it other)
                                                                            header-order
                                                                            row)))))
                         (data-noname (map-delete paired-data :name)))
                    (cl-pushnew (cons (plist-get paired-data :name) data-noname)
                                (symbol-value dbsymbl)
                                :test #'string= :key #'car)))))))
    ;; Reprocess recipes
    (if (eq type 'recipes)
        (if (eq (type-of (plist-get (cdar org-calories-db--recipes) :ingredients)) 'string)
            (setq org-calories-db--recipes
                  (--map (cons (car it) (org-calories-db--recipes2plist (cdr it)))
                         org-calories-db--recipes))))))


(defun org-calories-db--kill-table ()
  "Delete the current table."
  (forward-line 2) ;; to data line
  ;; erase current table
  (let ((start (line-beginning-position))
        (ended (progn (re-search-forward "^\\($\\||\s+|\\)")
                      (line-end-position))))
    (kill-region start ended)
    (insert "| |")
    (org-table-align)
    (org-table-goto-column 1)))

(defun org-calories-db--trimandsort (&optional reverse)
  "Trim table and sort on name, optionally in REVERSE."
  ;; Trim last empty row
  ;; Sort by name
  (save-excursion
    (goto-char (org-table-end))
    (forward-line -1)
    (org-table-goto-column 1)
    (org-table-kill-row)
    (org-table-sort-lines nil (if reverse ?A ?a))
    ;;
    (goto-char (org-table-end))
    (insert "\n"))) ;; preserve spacing


(defun org-calories-db--sync (type)
  "Sync db TYPE back to database file."
  (org-calories-db--makeheaders)
  (with-current-buffer (find-file-noselect org-calories-db-file)
    (save-excursion    ;; Parse Tables
      (goto-char 0)
      (let ((sstring nil)
            (dbsymbl nil))
        (cond ((eq type 'foods) (setq sstring org-calories-db--str-dbfood
                                      dbsymbl 'org-calories-db--foods))
              ((eq type 'recipes) (setq sstring org-calories-db--str-dbrecp
                                        dbsymbl 'org-calories-db--recipes))
              ((eq type 'exercises) (setq sstring org-calories-db--str-dbexer
                                          dbsymbl 'org-calories-db--exercises))
              (t (user-error "Database type doesn't exist")))
        (if (search-forward sstring nil t)   ;; General
            (when (re-search-forward org-table-line-regexp nil t)
              (unless (symbol-value dbsymbl)
                (user-error "Database not populated, quitting"))
              ;; Get the existing order of keywords from the table header
              (let ((header-order (--map (intern it) (car (org-table-to-lisp)))))
                (org-calories-db--kill-table)
                (setf (buffer-substring (line-beginning-position) (line-end-position)) "")
                ;; Dump current database
                (dolist (entry (--map (cons :name it)
                                      (--sort (string-lessp (car it)(car other)) (symbol-value dbsymbl)))) ;; rows
                  (dolist (keyw header-order)                                    ;; columns
                    (let ((am (plist-get entry keyw)))
                      (insert (format (if (floatp am) "| %.1f " "| %s ") am))))
                  (insert "|\n")))  ;;(org-table-next-field))))
              (when (re-search-backward org-table-line-regexp nil t)
                (org-table-align))))))
    (save-buffer)
    (message "synced %s to %s" type org-calories-db-file)))


(provide 'org-calories-db)
;;; org-calories-db.el ends here
