;;; LISPmäßig ist es viel angenehmer, am Anfang der Listen zu arbeiten statt am Ende
;;; ich arbeite deswegen mit den umgedrehten Listen!

(defparameter shortl '(10 13 16 21 30 45))
(defparameter longl '((0 3 6 9 12 15)
		      (1 3 6 10 15 21)
		      (10 13 16 21 30 45)))

(defun difflist (liste)
  "Differenzliste: Länge n-1; große Werte am Anfang"
  (unless (= 1 (length liste))
    (cons (- (car liste)
	     (cadr liste))
	  (difflist (cdr liste)))))

(defun alleZero (liste)
  (every #'zerop liste))

(defun diffdown (liste)
  "Liste aller Differenzlisten erstellen"
  (if (alleZero liste)
      (list liste)
    (append (list liste)
	    (diffdown (difflist liste)))))

(defun diffSumme (listlist)
  "Neuen ersten Wert berechnen, Summer aller CAR"
  (loop for item in listlist
	sum (car item)))

(defun main (langeliste)
  "Summe über alle Zeilen bilden"
  (loop for item in langeliste
	sum (diffSumme (diffdown (reverse item)))))
