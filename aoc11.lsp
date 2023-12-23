;;; Advent of Code 2023: Day 11

(defvar *datarray*)
(defparameter *zeilen* 10 "Ursprungsarray: Zeilenzahl")
(defparameter *spalten* 10 "Ursprungsarray: Spaltenzahl")

(defparameter *locherx* nil)
(defparameter *lochery* nil)

(defparameter testdaten '(
(
nil nil nil 1   nil nil nil nil nil nil 
)
(
nil nil nil nil nil nil nil 1   nil nil 
)
(
1   nil nil nil nil nil nil nil nil nil 
)
(
nil nil nil nil nil nil nil nil nil nil 
)
(
nil nil nil nil nil nil 1   nil nil nil 
)
(
nil 1   nil nil nil nil nil nil nil nil 
)
(
nil nil nil nil nil nil nil nil nil 1   
)
(
nil nil nil nil nil nil nil nil nil nil 
)
(
nil nil nil nil nil nil nil 1   nil nil 
)
(
1   nil nil nil 1   nil nil nil nil nil 
)
)
	      "enthält die zu verarbeitenden Daten (mit awk transformiert)")

;; Daten aufbereiten:

(setq *datarray* (make-array (list *zeilen* *spalten*) :initial-contents testdaten))


(defun sparseDat () ;funktioniert nicht
  (loop for i below *zeilen*
	for j below *spalten*
	collect (list i j)))

(defun DataExtract (daten)
  "SparseMatrix: Suche im Doppelarray die 1, return Liste der Koordinatenpaare"
  (let ((erg nil))
    (dotimes (i *zeilen* erg) ;Rückgabe ergebnis
      (dotimes (j *spalten*)
	(unless (null (aref daten i j))
	  (setq erg (cons (list i j)
			  erg)))))))


(defun findLocherX (datenpaare)
  (set-difference (loop for i
			below *zeilen*
			collect i)
		  (loop for x
			in  datenpaare
			collect (car x))))


(defun findLocherY (datenpaare)
  (set-difference (loop for i
			below *spalten*
			collect i)
		  (loop for x
			in  datenpaare
			collect (cadr x))))

(defun helpcarx (datenpaare grz)
  (mapcar #'(lambda (x) (list (if (> (car x) grz)
				  (1+ (car x))
				(car x))
			      (cadr x)))
	  datenpaare))

(defun ExpandUniverseX (datenpaare loecher);setzt voraus, dass die loecher absteigend geordnet sind!
  (if (null loecher) datenpaare
    (ExpandUniverseX (helpcarx datenpaare (car loecher))
		     (cdr loecher))))


(defun helpcarY (datenpaare grz)
  ""
  (mapcar #'(lambda (x) (list (car x)
			      (if (> (cadr x) grz)
				  (1+ (cadr x))
				(cadr x))))
	  datenpaare))

(defun ExpandUniverseY (datenpaare loecher);setzt voraus, dass die loecher absteigend geordnet sind!
  (if (null loecher) datenpaare
    (ExpandUniverseY (helpcary datenpaare (car loecher))
		     (cdr loecher))))

(defun PrepareData (datenarray)
  "Die Daten vollständig aufbereiten, incl. Expansion des Universums"
  (let ((daten nil))
    (setq daten (DataExtract datenarray))
    (setq *locherx* (findLocherX daten))
    (setq *lochery* (findLocherY daten))
    (setq *zeilen* (+ *zeilen* (length *locherx*)))
    (setq *spalten* (+ *spalten* (length *lochery*)))
    (ExpandUniverseX (ExpandUniverseY daten *lochery*) *locherx*)))


;;; eigentliches Programm:

(defun dist (g1 g2)
  "Abstand zweier Punkte (nicht-euklidisch)"
  (+ (abs (- (car g1) (car g2)))
     (abs (- (cadr g1) (cadr g2)))))
      
(defun Produkt (liste) ;hier nicht verwendet
  "Cartesisches Produkt der Liste mit sich selbst"
  (loop for x in liste nconc
        (loop for y in liste collect (list x y) )))

(defun Kombis (liste)
  "Alle Kombinationen des ersten Elements mit allen anderen"
  (mapcar #'(lambda (x) (list (car liste) x))
	  (cdr liste)))

(defun AlleKombis (liste)
  "Alle Kombinationen der Elemente der Liste"
  (unless (null liste)
    (append (Kombis liste)
	    (AlleKombis (cdr liste)))))

(defun AlleDists (liste)
  "Alle Abstände in einer Liste von Punkten"
  (mapcar #'(lambda (x) (dist (car x)
			      (cadr x)))
	  liste))

(defun MainDist (daten)
  (apply #'+ (AlleDists (AlleKombis daten))))


;;starte mit (MainDist (PrepareData *datarray*))



