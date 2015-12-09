;;taille d'un coté d'un carré
(defparameter +SIZE+ 3)

;;Convertit A - I en 0 - 9
(defparameter +tabHash+
  (make-hash-table :test #'eq))

(defun creer-grid (size)
"Créé une grille de taille (size*size)*(size*size)"
  (make-array (list (* size size) (* size size))))

;;Grille du sudoku
(defparameter +grid+
  (creer-grid +SIZE+))

;;Grille qui définit quelles cases sont modifiables
(defparameter +grid-access+
  (creer-grid +SIZE+))

(defparameter *grid-de-test* 
  (make-array '(9 9) :initial-contents
	      '((0 4 0 0 0 5 0 1 6)
		(0 0 7 0 6 3 5 0 0)
		(0 6 0 0 0 0 0 0 0)
		(0 0 0 0 5 0 3 2 9)
		(0 5 1 0 3 0 8 6 0)
		(3 2 6 0 4 0 0 0 0)
		(0 0 0 0 0 0 0 9 0)
		(0 0 5 6 8 0 7 0 0)
		(7 8 0 3 0 0 0 5 0))))


(defparameter +coups-possibles+
  (make-array (list (* +size+ +size+) (* +size+ +size+))))

(defparameter +cases-coups-possibles+
  (list))

(defun init-coups-possibles ()
  (do ((i 0 (1+ i)))
      ((> i (1- (* +size+ +size+))))
    (do ((j 0 (1+ j)))
	((> j (1- (* +size+ +size+))))
      (setf (aref +coups-possibles+ i j) (list))
      (if (= (aref +grid-access+ i j) 0)
	  (do ((k 1 (1+ k)))
	      ((> k 9))
	    (if (test-ligne-colonne-carre j i k)
		(setf (aref +coups-possibles+ i j)
		      (append (aref +coups-possibles+ i j) 
			      (list k)))))))))

(defun init-cases-coups-possibles ()
  (do ((i 0 (1+ i)))
      ((> i (1- (* +size+ +size+))))
    (do ((j 0 (1+ j)))
	((> j (1- (* +size+ +size+))))
      (if (not (endp (aref +coups-possibles+ i j)))
	  (setf +cases-coups-possibles+
		(append +cases-coups-possibles+ 
			(list (list i j))))))))

(defun initialiser-hash(size)
  (if (> size 26)
      NIL
      (initialiser-hash-term 
       size 
       (subseq 
	'(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z) 
	0 size))))

(defun initialiser-hash-term(size list)
  (setf (gethash (car (last list)) +tabHash+) size)
  (if (= 1 size)
      T
      (initialiser-hash-term (1- size) (butlast list))))

(defun init-all(size)
"Initialise tout le sudoku"
  (initialiser-hash (* size size))
  (if (not (= size +SIZE+))
      (progn 
	(setf +grid+ (creer-grid size))
	(setf +grid-access+ (creer-grid size)))
      T))
      

(defun set-tile(largeur hauteur valeur)
  "Attribue à la case de coordonnée (largeur, hauteur) de +grid+ valeur, si valeur n'est pas déjà dans la ligne, colonne ou carré"
  (let ((taille (car (array-dimensions +grid+)))
	(l (1- (getHash largeur +tabHash+)))
	(h (1- hauteur)))
    (if (eq (aref +grid-access+ h l) 0)
	 (if (test-ligne-colonne-carre l h valeur)
	     (if (or (or (< valeur 1) (> valeur taille))
		     (> h taille)
		     (> l taille))
		 NIL
		 (setf (aref +grid+ h l) valeur))
	     (format t "Impossible de mettre cette valeur dans cette case. ~%"))
	 (format t "Cette case n'est pas accessible. ~%"))))

(defun test-ligne-colonne-carre(largeur hauteur valeur)
"Lance les trois test, return T si on peut mettre valeur dans la case largeur hauteur, NIL sinon."
  (let ((longueur (1- (* +size+ +size+))))
    (if (and (test-ligne longueur hauteur valeur)
	     (test-colonne largeur longueur valeur)
	     (test-carre largeur hauteur valeur))
	T
	NIL)))

(defun test-ligne(n h valeur)
"Test si valeur n'est pas déjà comprise dans la ligne d'ordonnee h, n est le nombre de case dans une ligne"
  (if (< n 0)
      T
      (if (= (aref +grid+ h n) valeur)
	  NIL
	  (test-ligne (1- n) h valeur))))

(defun test-colonne(l n valeur)
"Test si valeur n'est pas déjà comprise dans la colonne d'abscisse l, n est le nombre de case dans une colonne"
  (if (< n 0)
      T
      (if (= (aref +grid+ n l) valeur)
	  NIL
	  (test-colonne l (1- n) valeur))))


(defun test-carre(l h valeur)
"Test si valeur n'est pas déjà comprise dans le carré qui contient la case de coordonnée (l,h)"
  (let ((x (* (floor (/ l +SIZE+)) +SIZE+))
	(y (* (floor (/ h +SIZE+)) +SIZE+)))
    (test-carre-recX (1- +SIZE+) x y valeur)))

(defun test-carre-recX(n x y valeur)
  (if (< n 0)
      T
      (if (test-carre-recY(1- +SIZE+) (+ n x) y valeur)
	(test-carre-recX (1- n) x y valeur)
	NIL)))

(defun test-carre-recY(n x y valeur)
  (if (< n 0)
      T
      (if (= (aref +grid+ (+ y n) x) valeur)
	  NIL
	  (test-carre-recY (1- n) x y valeur))))

(defun copy-grid(grid)
  "Copy la grid passé en paramètre dans la grid globale."
  (setf +size+ (floor (sqrt (car (array-dimensions grid)))))
  (init-all +size+)
  (copy-grid-recX grid 0))

(defun copy-grid-recX(grid l)
  (let ((longueur (* +size+ +size+)))
    (if (< l longueur)
	(progn
	  (copy-grid-recY grid l 0)
	  (copy-grid-recX grid (1+ l))))))

(defun copy-grid-recY(grid l h)
  (let ((longueur (* +size+ +size+)))
    (if (< h longueur)
	(progn
	  (if (not (eq (aref grid l h) 0))
	      (progn
		(setf (aref +grid+ l h) (aref grid l h))
		(setf (aref +grid-access+ l h) 1)))
	  (copy-grid-recY grid l (1+ h))))))
	
    
(defun print-grid()
  "Affiche la grille"
  (let ((alphabet 
	 '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))
    (format t "~a" "  ")
    (do ((i 0 (1+ i)))
	((>= i (* +size+ +size+)))
      (print-grid-separateur-colonne i)
      (format t "~a" (nth i alphabet))
      (format t "~a" " ")))
  (format t "|~%" )
  (print-grid-separateur-ligne 0)
 
  (do ((i 1 (1+ i)))
      ((> i (* +size+ +size+)))
    (print-grid-ligne i)
    (print-grid-separateur-ligne i)))

(defun print-grid-ligne(h)
  (format t "~a" h)
  (format t "~a" " ")
  (do ((i 0 (1+ i)))
      ((>= i (* +size+ +size+)))
    (print-grid-separateur-colonne i)
    (if (not (eq (aref +grid+ (1- h) i) 0))
	(format t "~a" (aref +grid+ (1- h) i))
	(format t "~a" "."))
    (format t "~a" " "))
  (format t "|~%"))
    

(defun print-grid-separateur-colonne(i)
  (if (eq (mod i +size+) 0)
      (format t "~a" "| ")))

(defun print-grid-separateur-ligne(i)
  (if (eq (mod i +size+) 0)
      (progn
	(do ((i 0 (1+ i)))
	    ((>= i (+ (* +size+ +size+) 4 (1- +size+))))
	  (format t "- "))
	(format t " ~%"))))

(defun isFinish()
  (do ((i 0 (+ 1 i)))
      ((>= i (* +size+ +size+)))
    (do ((j 0 (+ 1 j)))
	((>= j (* +size+ +size+)))
      (if (eq (aref +grid+ i j) 0)
	  (return-from isFinish) NIL)))
  T)


;;; stratégie aléatoire

(defun init-standalone (grid)
  (copy-grid grid)
  (init-coups-possibles)
  (init-cases-coups-possibles))  


(defun main-standalone ()
  (let ((longueur (length +cases-coups-possibles+))
	(coord NIL)
	(val NIL))
    (do ()
	((= longueur 0))
      (setf coord 
	    (nth (random longueur) +cases-coups-possibles+))
      (setf val 
	    (nth  (random (length (aref +coups-possibles+
					   (first coord)
					   (second coord))))
		  (aref +coups-possibles+ 
		       (first coord) 
		       (second coord))))
      (do ((i 0 (1+ i)))
	  ((>= i (* +size+ +size+)))
	(setf (aref +coups-possibles+
		    i
		    (second coord))
	      (remove val (aref +coups-possibles+
				i
				(second coord)) :test #'equal)))

      (do ((j 0 (1+ j)))
	  ((>= j (* +size+ +size+)))
	(setf (aref +coups-possibles+
		  (first coord)
		  j)
	    (remove val (aref +coups-possibles+
			      (first coord)
			      j) :test #'equal))
	(if (endp (aref +coups-possibles+ 
			(first coord) 
			j))
	    (progn
	      (setf +cases-coups-possibles+
		    (remove (list (first coord) j)
				  +cases-coups-possibles+ 
			    :test #'equal))
	      (setf longueur (1- longueur)))))

      (do ((i (- (first coord) 
		 (mod (first coord) +size+)) 
	      (1+ i)))
	  ((>= i (+ (mod (first coord) +size+) +size+)))
	(do ((j (- (second coord) 
		   (mod (second coord) +size+)) 
		(1+ j)))
	    ((>= j (+ (mod (second coord) +size+) +size+)))
	  (setf (aref +coups-possibles+ i j)
	    (remove val (aref +coups-possibles+ i j) 
		    :test #'equal))
	  (if (endp (aref +coups-possibles+ 
		      i 
		      j))
	  (progn
	    (setf +cases-coups-possibles+
		  (remove (list i j) +cases-coups-possibles+ 
			  :test #'equal))
	    (setf longueur (1- longueur))))))

      
      
	  
      (setf (aref +grid+ (first coord) (second coord)) val)
      (print-grid))))
      
		       
    
  

  
  
    
  
  
      
      
  
