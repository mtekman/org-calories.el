;;; org-calories-timestring.el --- Functions to be performed on time strings -*- lexical-binding: t; -*-

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
(defun org-calories-timestring--to-hash (date)
  "Convert DATE to a unique hash."
  (format "%s" (--reduce
                (concat (format "%s" acc) (format "%02d" it))
                (org-calories-timestring--to-integers date))))

(defun org-calories-timestring--to-integers (timestr)
  "Convert TIMESTR <2020-06-14> or <2020-06-14 Wed 16:15> to list (2020 6 14 16 15)."
  (--filter (> it 0)(--map (string-to-number it)
                           (split-string timestr "[-<>: ]"))))

(defun org-calories-timestring--lteq (timelst1 timelst2)
  "TIMELST1 less than or equal to TIMELST2?"
  (and
   (and (<= (car timelst1) (car timelst2))        ;; year
        (<= (cadr timelst1) (cadr timelst2))      ;; month
        (<= (caddr timelst1) (caddr timelst2)))   ;; day
   (if (eq (length timelst1) 3) t
     (and (<= (cadddr timelst1) (cadddr timelst2))                 ;; hour
          (<= (cadddr (cdr timelst1)) (cadddr (cdr timelst2))))))) ;; minute

(defun org-calories-timestring--eq (timelst1 timelst2)
  "TIMELST1 equal to TIMELST2?"
  (and
   (and (= (car timelst1) (car timelst2))        ;; year
        (= (cadr timelst1) (cadr timelst2))      ;; month
        (= (caddr timelst1) (caddr timelst2)))   ;; day
   (if (eq (length timelst1) 3) t
     (and (= (cadddr timelst1) (cadddr timelst2))                 ;; hour
          (= (cadddr (cdr timelst1)) (cadddr (cdr timelst2))))))) ;; minute

(provide 'org-calories-timestring)
;;; org-calories-timestring.el ends here
