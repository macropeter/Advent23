;;; Advent of Code 2023: Day 11

(defparameter data '((0 4) ;Liste von Punkten
	      (1 9)
	      (2 0)
	      (5 8)
	      (6 1)
	      (7 12)
	      (10 9)
	      (11 0)
	      (11 5)))

(defun dist (g1 g2)
  "Abstand zweier Punkte"
  (+ (abs (- (car g1) (car g2)))
     (abs (- (cadr g1) (cadr g2)))))
      
(defun Produkt (liste)
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

(defun MainDist ()
  (apply #'+ (AlleDists (AlleKombis data))))
