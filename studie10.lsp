;; Suche in einem binären Baum
;; Studie zu AoC 10

(defparameter *Tree*  ;Beispiel
   '(1
    (2
     (4 (7))
     (5))
    (3
     (6 (8)
	(9))) ) )



(defun binsuche (baum)
  (let ((nr 0))  ;Closure
    (labels ((preorder (Tree n)
	    (if Tree
		(progn
		  (preorder (cadr Tree) (1+ n))
		  (preorder (caddr Tree) (1+ n))
		  (setq nr (max nr n))
		  nr)))))
    (preorder baum 1)))
