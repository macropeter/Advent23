(defparameter *spalten* 10)
(defparameter *zeilen* 10)
(defvar *plat* (make-array '(10 10))); dient als globale Variable
(defparameter aplat '(
(
1   nil nil nil nil 0   nil nil nil nil 
)
(
1   nil 1   1   0   nil nil nil nil 0   
)
(
nil nil nil nil nil 0   0   nil nil nil 
)
(
1   1   nil 0   1   nil nil nil nil 1   
)
(
nil 1   nil nil nil nil nil 1   0   nil 
)
(
1   nil 0   nil nil 1   nil 0   nil 0   
)
(
nil nil 1   nil nil 0   1   nil nil 1   
)
(
nil nil nil nil nil nil nil 1   nil nil 
)
(
0   nil nil nil nil 0   0   0   nil nil 
)
(
0   1   1   nil nil 0   nil nil nil nil 
)
))


;;https://stackoverflow.com/questions/9549568/common-lisp-convert-between-lists-and-arrays
(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))


(setq *plat* (list-to-2d-array
	      (reverse aplat))) ;leichter nach unten arbeiten als nach oben


(defun zeige (plf)
  (loop for i from 0 to (1- *spalten*) do
	(loop for j from 0 to (1- *zeilen*) do
	      (if (null (aref plf i j))
		  (princ #\.)
		(princ (aref plf i j))))
	(terpri)))



(defun steige-ab (zeile spalte)
  "der Einser auf (zeile/spalte) steigt auf den freien Platz ab"
  (progn 
    (setf (aref *plat* (1+ zeile) spalte) 1)
    (setf (aref *plat* zeile spalte) nil)))
    
(defun all-the-way-down (zeile spalte)
  (when (and (< zeile (1- *zeilen*)) ;Ende, falls schon ganz unten (Kurzschlussauswertung!)
	     (null (aref *plat* (1+ zeile) spalte))) ;prüfen ob unterhalb frei ist
    (steige-ab zeile spalte) ;tauschen
    (all-the-way-down (1+ zeile) spalte))) ;weiter absteigen

(defun check-spalte (spalte)
  "Geht eine Spalte von unten nach oben durch"
  (loop for i
	from (- *zeilen* 2) ;die letzte Zeile braucht nicht geprüft werden
	downto 0
	do (let ((hilf (aref *plat* i spalte)))
	     (if (and (not (null hilf))
		      (= hilf 1))
		 (all-the-way-down i spalte)))))

(defun shift-north ()
  "Checkt alle Spalten von 0 bis 9 und ordnet sie"
  (loop for j
	from 0
	to (1- *zeilen*)
	do (check-spalte j)))

(defun SummeZeile (zeile)
  "Summe der 1 in einer Zeile"
  (loop for i
	from 0
	to (1- *spalten*)
	sum (let ((hilf (aref *plat* zeile i)))
	      (if (null hilf)
		  0
		hilf))))

(defun MainSumme ()
  "Gewichtete Summe über alle Zeilen"
  (loop for j
	from 0
	to (1- *zeilen*)
	sum (* (1+ j)
	       (SummeZeile j))))
