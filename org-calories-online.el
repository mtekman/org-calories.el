
(setq org-calories-online--base "https://fddb.info/xml/ac/1/en")

(defun org-calories-search (query)
  (let* ((reftitle nil)
         (search-url (format "%s/%s"
                             org-calories-online--base
                             query))
         (results-buffer (url-retrieve-synchronously
                          search-url nil t 5)))
    (with-current-buffer results-buffer
      (goto-char 0)
      (while (re-search-forward
              "window.location.href='\\([^']*\\)';\"><div id='acelement.'>\\([^<]*\\)<" nil t)
        (let ((href (buffer-substring-no-properties (match-beginning 1)
                                                    (match-end 1)))
              (titl (buffer-substring-no-properties (match-beginning 2)
                                                    (match-end 2))))
          (push (list titl href (org-calories-getinfo href)) reftitle))))
    reftitle))

(defun org-calories-getinfo (url)
  "Retrieve calorific information from URL."
  (let* ((tabdata nil)
         (results-buffer (url-retrieve-synchronously url nil t 5)))
    (with-current-buffer results-buffer
      (goto-char 0)
      (when (search-forward "itemsec2012" nil t)
        ;; Header
        (if (re-search-forward "<h2[^>]*>Data for\\([^<]*\\)</h2>" nil t)
            (push (cons "Portion"
                        (buffer-substring-no-properties (match-beginning 1)
                                                        (match-end 1)))
                  tabdata))
        ;; Data
        (while (re-search-forward
                ">\\([^<]*\\)</\\(a\\|span\\)></div><div>\\([^<]*\\)</div>"
                nil t)
          (let ((item (buffer-substring-no-properties (match-beginning 1)
                                                      (match-end 1)))
                (valu (buffer-substring-no-properties (match-beginning 3)
                                                      (match-end 3))))
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

(defun dino-xml-pretty-print-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
    http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
    this. The function inserts linebreaks to separate tags that have
    nothing but whitespace between them. It then indents the markup
    by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    ;; split <foo><bar> or </foo><bar>, but not <foo></foo>
    (goto-char begin)
    (while (search-forward-regexp ">[ \t]*<[^/]" end t)
      (backward-char 2) (insert "\n") (incf end))
    ;; split <foo/></foo> and </foo></foo>
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
      (backward-char) (insert "\n") (incf end))
    ;; put xml namespace decls on newline
    (goto-char begin)
    (while (search-forward-regexp "\\(<\\([a-zA-Z][-:A-Za-z0-9]*\\)\\|['\"]\\) \\(xmlns[=:]\\)" end t)
      (goto-char (match-end 0))
      (backward-char 6) (insert "\n") (incf end))
    (indent-region begin end nil)
    (normal-mode))
  (message "All indented!"))


(defun dino-xml-pretty-print-buffer ()
  "pretty print the XML in a buffer."
  (interactive)
  (dino-xml-pretty-print-region (point-min) (point-max)))
