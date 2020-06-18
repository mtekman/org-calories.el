;;; org-calories-online.el --- Search online for Food information -*- lexical-binding: t; -*-

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
(defconst org-calories-online--base "https://fddb.info/xml/ac/1/en")


(defun org-calories-online-search (qsearch)
  "Search online for food query QSEARCH."
  (interactive "sFood Query: ")
  (let* ((datalist (org-calories-online--search qsearch))
         (userres (completing-read (format "Food options for '%s' (hit TAB)\n" qsearch)
                                   (--map (plist-get it :user-options) datalist)
                                   nil t ""))
         (datamatch (car (--filter (string= userres (plist-get it :user-options))
                                   datalist))))
    datamatch))


(defun org-calories-online--search (query)
  "Search food QUERY."
  (let* ((reftitle nil)
         (search-url (format "%s/%s" org-calories-online--base query))
         (results-buffer (url-retrieve-synchronously search-url nil t 5)))
    ;; Populate list
    (with-current-buffer results-buffer
      ;;(write-file "testsearch.txt") ;; debug
      (goto-char 0)
      (while (re-search-forward
              "window.location.href='\\([^']*\\)';\"><div id='acelement.'>\\(.*+?\\)</div>" nil t)
        (let* ((href (buffer-substring-no-properties (match-beginning 1)
                                                     (match-end 1)))
               (titl (buffer-substring-no-properties (match-beginning 2)
                                                     (match-end 2)))
               (titl (mapconcat 'identity
                                (--filter (not(member it '("" "b")))
                                          (split-string titl "[</>]")) "")))
          (push (list titl href (org-calories-online--extractrelevant
                                 (org-calories-online--getinfo href)))
                reftitle)))
      ;; Format list
      (let* ((widthspacing (- (frame-width) (+ 8 5 4 4 4 4 4 5 11)))
             (title-format (format "%%%ds" (- widthspacing)))
             (form-string (concat title-format "\t%8s %5s %4s (%4s %4s) %4s %4s %5s"))
             (food-matches
              (--map (list :food (car it)
                           :food-info (nth 2 it)
                           :user-options
                           (format form-string
                                   (if (> (length (car it)) widthspacing)
                                       (substring (car it) 0 widthspacing)
                                     (car it))
                                   (format "%3d%s" (plist-get (nth 2 it) :amount)
                                           (plist-get (nth 2 it) :unit))
                                   (format "%3dkC" (plist-get (nth 2 it) :kc))
                                   (format "%3dg" (plist-get (nth 2 it) :carbs))
                                   (format "%3dg" (plist-get (nth 2 it) :sugars))
                                   (format "%3dg" (plist-get (nth 2 it) :fibre))
                                   (format "%3dg" (plist-get (nth 2 it) :protein))
                                   (format "%3dg" (plist-get (nth 2 it) :fat))
                                   ;;(format "%3dg" (plist-get (nth 2 it) :sat)) -- not relevant here
                                   (format "%3dg" (plist-get (nth 2 it) :salt))))
                     reftitle)))
        (push (list :food "--" :food-info nil
                    :user-options (format form-string
                                          (make-string widthspacing ?-)
                                          "Amount" "kCal" "Carb" "Sug" "Fib" "Pro" "Fat" "Salt"))
              food-matches)
        food-matches))))


(defun org-calories-online--inlineextract (key pinfo)
  "Extract number and unit from KEY extracted from PINFO."
  (let ((val (alist-get key pinfo nil nil #'string=)))
    (if val
        (let* ((kvalunit (split-string val))
               (kval (string-to-number (car kvalunit)))
               (kunt (cadr kvalunit)))
          (if (member kunt '("kcal" "g" "mg" "ml"))
              (cons kval kunt)
            (debug key pinfo)
            (user-error "Could not parse %d %s" kval kunt)))
      (cons 0 "g"))))



(defun org-calories-online--extractrelevant (info)
  "Extract relevant calorific info from INFO derived from --getinfo."
  (let ((calorie (org-calories-online--inlineextract "Calories" info))
        (pamount (org-calories-online--inlineextract "Portion" info))
        (totcarb (org-calories-online--inlineextract "Carbohydrates" info))
        (cafibre (org-calories-online--inlineextract "Dietary fibre" info))
        (casugar (org-calories-online--inlineextract "thereof Sugar" info))
        (protein (org-calories-online--inlineextract "Protein" info))
        (fatamnt (org-calories-online--inlineextract "Fat" info))
        (saltamt (org-calories-online--inlineextract "Salt" info)))
    (list :amount (car pamount) :unit (cdr pamount)
          :kc (car calorie)
          :carbs (car totcarb) :fibre (car cafibre) :sugars (car casugar)
          :protein (car protein) :fat (car fatamnt)
          :sat (/ (car fatamnt) 2)  ;; is not captured on this website, so we assume 50%
          :salt (car saltamt))))


(defun org-calories-online--getinfo (url)
  "Retrieve calorific information from URL."
  (let* ((tabdata nil)
         (get-str #'buffer-substring-no-properties)
         (results-buffer (url-retrieve-synchronously url nil t 5)))
    (with-current-buffer results-buffer
      (goto-char 0)
      (when (search-forward "itemsec2012" nil t)
        ;; Header
        (if (re-search-forward "<h2[^>]*>Data for\\([^<]*\\)</h2>" nil t)
            (push (cons "Portion"
                        (funcall get-str (match-beginning 1) (match-end 1)))
                  tabdata))
        ;; Data
        (while (re-search-forward
                ">\\([^<]*\\)</\\(a\\|span\\)></div><div>\\([^<]*\\)</div>"
                nil t)
          (let ((item (funcall get-str (match-beginning 1) (match-end 1)))
                (valu (funcall get-str (match-beginning 3) (match-end 3))))
            (push (cons item valu) tabdata))))
      tabdata)))

(provide 'org-calories-online)
;;; org-calories-online.el ends here

