(defparameter *TimeList* '(7 15 30))
(defparameter *DistanceList* '(9 40 200))

(defun renn (grenze)
  (loop for i from 0
       to grenze
       collect (* i (- grenze i))))


(defun filtern (zeit abstand)
  (remove-if #'(lambda (x) (<= x abstand))
	     (renn zeit)))

(defun berechneAoC6 (Zeitliste Abstandliste)
  (apply '* (loop for k in Zeitliste
		  for l in Abstandliste
		  collect (length (filtern k l)))))

(defun Main ()
  (berechneAoC6 *TimeList* *DistanceList*))
