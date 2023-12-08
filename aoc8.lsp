(defparameter *daten*
	      '((:AAA :BBB :BBB)
		(:BBB :AAA :ZZZ)
		(:ZZZ :ZZZ :ZZZ)))

(defparameter *kurs*
	      (string "LLR"))

(defun charMod (str nr)
  "Zugriff auf den unendlich fortgesetzen String"
  (char str (mod nr (length str))))

(defun suche (x dat)
  "Suche einen Knoten, gib die zwei Wege zurück"
  (if (equal (caar dat)
	     x)
      (cdar dat) ;gib den CDR dieses Knotens zurück
    (suche x (cdr dat)))) ;suche weiter

(defun BestimmeNextNode (knoten nr)
  "Bestimme den nächsten Knoten"
  (let ((hilf (suche knoten *daten*))) ;suche den Knoten in der Liste
    (if (eq (charMod *kurs* nr) #\L)
	(car hilf) ;linker Knoten weiter
      (cadr hilf)))) ; weiter zum rechten Knoten

(defun SuchePfad (anfang nr)
  (let ((hilf (BestimmeNextNode anfang nr)))
    (if (eq hilf :ZZZ) ;wenn :ZZZ, Ende erreicht
	nr
      (SuchePfad hilf (1+ nr))))) ;suche weiter, mit dem neuen String

(defun Start ()
  (SuchePfad :AAA 1))
