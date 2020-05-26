
;;; Code:
(defvar org-calories-online--base "https://fddb.info/xml/ac/1/en")

(defun org-calories-testoptions (qsearch)
  "Search query QSEARCH."
  (completing-read "Food options:\n"
                   (--map (plist-get it :user-options)
                          (org-calories-online--search qsearch))
                   nil t))


(defun org-calories-online--search (query)
  "Search food QUERY."
  (let* ((reftitle nil)
         (search-url (format "%s/%s" org-calories-online--base query))
         (results-buffer (url-retrieve-synchronously search-url nil t 5)))
    ;; Populate list
    (with-current-buffer results-buffer
      (goto-char 0)
      (while (re-search-forward
              ;; TODO: <b>bold</b> elements are also terminal nodes in match 2
              "window.location.href='\\([^']*\\)';\"><div id='acelement.'>\\([^<]*\\)<" nil t)
        (let ((href (buffer-substring-no-properties (match-beginning 1)
                                                    (match-end 1)))
              (titl (buffer-substring-no-properties (match-beginning 2)
                                                    (match-end 2))))
          (push (list titl href (org-calories-online--extractrelevant
                                 (org-calories-online--getinfo href)))
                reftitle)))
      ;; Format list
      (--map (list :food (car it)
                   :food-info (nth 2 it)
                   :user-options
                   (format "%s -- [%s %s %s (%s %s) %s %s %s]"
                           (car it)
                           (format "%3d%s" (plist-get (nth 2 it) :amount)
                                   (plist-get (nth 2 it) :unit))
                           (format "%3dkCal" (plist-get (nth 2 it) :kc))
                           (format "%3dg" (plist-get (nth 2 it) :carbs))
                           (format "%3dg" (plist-get (nth 2 it) :sugars))
                           (format "%3dg" (plist-get (nth 2 it) :fibre))
                           (format "%3dg" (plist-get (nth 2 it) :protein))
                           (format "%3dg" (plist-get (nth 2 it) :fat))
                           (format "%3dmg" (plist-get (nth 2 it) :sodium))))
                    reftitle))))


(defun org-calories-online--inlineextract (key pinfo)
  "Extract number and unit from KEY extracted from PINFO."
  (let ((val (alist-get key pinfo nil nil #'string=)))
    (if val
        (let* ((kvalunit (split-string val))
               (kval (string-to-number (car kvalunit)))
               (kunt (cadr kvalunit)))
          (if (member kunt '("kcal" "g" "mg"))
              (cons kval kunt)
            (user-error "Could not parse %d %s" kval kunt)))
      (cons 0 "g"))))



(defun org-calories-online--extractrelevant (info)
  "Extract relevant calorific info from INFO derived from --getinfo."
  (let* ((calorie (org-calories-online--inlineextract "Calories" info))
         (pamount (org-calories-online--inlineextract "Portion" info))
         (totcarb (org-calories-online--inlineextract "Carbohydrates" info))
         (cafibre (org-calories-online--inlineextract "Dietary fibre" info))
         (casugar (org-calories-online--inlineextract "thereof Sugar" info))
         (protein (org-calories-online--inlineextract "Protein" info))
         (fatamnt (org-calories-online--inlineextract "Fat" info))
         (sodiumv (org-calories-online--inlineextract "Sodium" info)))
    (list :amount (car pamount) :unit (cdr pamount)
          :kc (car calorie)
          :carbs (car totcarb) :fibre (car cafibre) :sugars (car casugar)
          :protein (car protein) :fat (car fatamnt)
          :sodium (or (car sodiumv) 0))))


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


;; (defun online-search (fname))

;; (defun online-parselist (results)
;;   "Parse web RESULTS into plists."
;;   (loop for res in results
;;         collect (online-parseresult res)))

;; (defcustom userformat-string "something"
;;   "Carb Fibre Sugars Fat Protein Sodium."
;;   :type 'string
;;   :group 'org-calories)

;; (defun online-userlist (plist)
;;   "Convert food PLIST into a description."
;;   (loop for pl in plist
;;         collect (format userformat-string plist)))

;; (defun online-retrieve (fname)
;;   "Search for FNAME in backend database"
;;   (let* ((results (online-search fname))
;;          (resplist (online-parselist results))
;;          (resoffer (online-userlist resplist)))
;;     (if resplist
;;         (read-answer (concat fname ": ") resoffer)
;;       (message "No results found."))))

(Calories . 220 kcal)
(Portion .   100 g)
(Carbohydrates . 37 g)
(Dietary fibre . 5.9 g)
(thereof Sugar . 1.2 g)
(Protein . 8.5 g)
(Fat . 4.4 g)

(Iodine . 1 mg)
(Fluorine . 0.01 mg)
(Copper . 0.2 mg)
(Phosphorus . 138 mg)
(Calcium . 23 mg)
(Potassium . 171 mg)
(Sulphur . 41 mg)
(Manganese . 1.3 mg)
(Chlorine . 670 mg)
(Magnesium . 46 mg)
(Zinc . 1.5 mg)
(Iron . 1.6 mg)
(Salt . 1.1481 g)
(Vitamin B6 . 0.08 mg)
(Riboflavin . 0.05 mg)
(Thiamine . 0.18 mg)
(Vitamin E . 0.7 mg)
(Water content . 43%)

(Calorific value . 921 kJ)
