;;taille d'un coté d'un carré
(defparameter +SIZE+ 3)

;;taille d'un coté de la grille
(defparameter +LONG-SIZE+ 9)

;;Convertit A - I en 0 - 9
(defparameter +TAB-HASH+
  (make-hash-table :test #'eq))

;;Créer l'alphabet
(defparameter +ALPHABET+
  '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

(defparameter *cmp* 0)

(defun creer-grid ()
"Créé une grille de taille (size*size)*(size*size)"
  (make-array (list +LONG-SIZE+ +LONG-SIZE+)))

;;Grille du sudoku
(defparameter +GRID+
  (creer-grid))

;;Grille qui définit quelles cases sont modifiables
(defparameter +GRID-ACCESS+
  (creer-grid))

;;grille de sudoku utilisée pour les tests.
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


(defparameter *grid-1*
  (make-array '(9 9) :initial-contents
	      '((0 1 9 0 2 0 0 0 0)
		(7 0 5 6 0 0 9 3 2)
		(0 0 2 0 0 8 0 7 1)
		(0 0 7 2 0 0 0 0 4)
		(0 3 8 1 4 0 0 2 7)
		(2 4 1 7 0 0 3 8 9)
		(0 5 6 0 9 2 0 0 3)
		(0 0 4 0 5 1 2 0 8)
		(0 0 3 8 0 6 4 0 0))))

(defparameter *grid-2*
  (make-array '(9 9) :initial-contents
	      '((0 2 5 0 3 8 0 9 0)
		(0 0 0 0 7 0 1 4 8)
		(0 0 8 0 6 0 5 3 0)
		(2 1 3 6 0 5 8 7 9)
		(6 0 0 2 0 7 3 1 4)
		(7 0 4 3 1 9 2 0 0)
		(9 3 7 8 0 1 0 6 0)
		(8 4 0 7 0 0 9 2 3)
		(5 6 2 4 9 0 7 8 1))))

(defparameter *grid-3*
  (make-array '(9 9) :initial-contents
	      '((3 0 7 0 0 4 5 6 0)
		(0 0 0 0 0 0 0 0 4)
		(0 0 4 0 0 7 0 3 0)
		(0 2 0 0 1 0 0 7 5)
		(0 0 5 7 0 0 9 0 0)
		(7 0 0 0 5 0 6 0 8)
		(9 0 3 0 2 0 0 8 7)
		(0 0 2 0 8 3 4 5 6)
		(8 0 0 0 7 5 2 9 3))))



;;tableau contenant tous les coups possibles pour chaque case
(defparameter +COUPS-POSSIBLES+
  (make-array (list +LONG-SIZE+ +LONG-SIZE+)))

;;liste contenant toutes les cases où un coup est possible
(defparameter +CASES-COUPS-POSSIBLES+
  (list))

(defun init-coups-possibles ()
"Initialise +coup-possibles+"
  (do ((i 0 (1+ i)))
      ((> i (1- +LONG-SIZE+)))
    (do ((j 0 (1+ j)))
	((> j (1- +LONG-SIZE+)))
      (setf (aref +COUPS-POSSIBLES+ i j) (list))
      (if (= (aref +GRID-ACCESS+ i j) 0)
	  (do ((k 1 (1+ k)))
	      ((> k 9))
	    (if (test-ligne-colonne-carre j i k)
		(setf (aref +COUPS-POSSIBLES+ i j)
		      (append (aref +COUPS-POSSIBLES+ i j) 
			      (list k)))))))))

(defun init-cases-coups-possibles ()
"Initialise +CASES-COUPS-POSSIBLES+"
  (do ((i 0 (1+ i)))
      ((> i (1- +LONG-SIZE+)))
    (do ((j 0 (1+ j)))
	((> j (1- +LONG-SIZE+)))
      (if (not (endp (aref +COUPS-POSSIBLES+ i j)))
	  (setf +CASES-COUPS-POSSIBLES+
		(append +CASES-COUPS-POSSIBLES+ 
			(list (list i j))))))))

(defun initialiser-hash()
"Initialise la table de hachage."
  (if (> +LONG-SIZE+ 26)
      NIL
      (initialiser-hash-term 
       +LONG-SIZE+ 
       (subseq +ALPHABET+ 0 +LONG-SIZE+))))

(defun initialiser-hash-term(size list)
"Utilisée dans initialiser-hash."
  (setf (gethash (car (last list)) +TAB-HASH+) size)
  (if (= 1 size)
      T
      (initialiser-hash-term (1- size) (butlast list))))

(defun init-all(size)
"Initialise tout le sudoku"
  (initialiser-hash)
  (if (not (= size +SIZE+))
      (progn 
	(setf +GRID+ (creer-grid))
	(setf +GRID-ACCESS+ (creer-grid)))
      T))
      
(defun test-ligne-colonne-carre(largeur hauteur valeur)
"Lance les trois test"
  (let ((longueur (1- +LONG-SIZE+)))
    (if (and (test-ligne longueur hauteur valeur)
	     (test-colonne largeur longueur valeur)
	     (test-carre largeur hauteur valeur))
	T
	NIL)))

(defun test-ligne(n h valeur)
"Test si valeur n'est pas déjà comprise dans la ligne d'ordonnee h, n est le nombre de case dans une ligne"
  (if (< n 0)
      T
      (if (= (aref +GRID+ h n) valeur)
	  NIL
	  (test-ligne (1- n) h valeur))))

(defun test-colonne(l n valeur)
"Test si valeur n'est pas déjà comprise dans la colonne d'abscisse l, n est le nombre de case dans une colonne"
  (if (< n 0)
      T
      (if (= (aref +GRID+ n l) valeur)
	  NIL
	  (test-colonne l (1- n) valeur))))


(defun test-carre(l h valeur)
"Test si valeur n'est pas déjà comprise dans le carré qui contient la case de coordonnée (l,h)"
  (let ((x (* (floor (/ l +SIZE+)) +SIZE+))
	(y (* (floor (/ h +SIZE+)) +SIZE+)))
    (test-carre-recX (1- +SIZE+) x y valeur)))

(defun test-carre-recX(n x y valeur)
"Utilisée dans test-carre, pour parcourir la ligne."
  (if (< n 0)
      T
      (if (test-carre-recY(1- +SIZE+) (+ n x) y valeur)
	(test-carre-recX (1- n) x y valeur)
	NIL)))

(defun test-carre-recY(n x y valeur)
"Utilisée dans test-carré-recX, pour parcourir la colonne."
  (if (< n 0)
      T
      (if (= (aref +GRID+ (+ y n) x) valeur)
	  NIL
	  (test-carre-recY (1- n) x y valeur))))

(defun copy-grid(grid)
  "Copy la grid passé en paramètre dans la grid globale."
  (setf +SIZE+ (floor (sqrt (car (array-dimensions grid)))))
  (init-all +SIZE+)
  (copy-grid-recX grid 0))

(defun copy-grid-recX(grid l)
"Utilisée dans copy-grid, pour parcourir la ligne."
  (if (< l +LONG-SIZE+)
      (progn
	(copy-grid-recY grid l 0)
	(copy-grid-recX grid (1+ l)))))

(defun copy-grid-recY(grid l h)
"Utilisée dans copy-grid-recX, pour parcourir la colonne."
    (if (< h +LONG-SIZE+)
	(progn
	  (if (not (eq (aref grid l h) 0))
	      (progn
		(setf (aref +GRID+ l h) (aref grid l h))
		(setf (aref +GRID-ACCESS+ l h) 1)))
	  (copy-grid-recY grid l (1+ h)))))
	


;;; stratégie aléatoire

(defun init-standalone (grid)
"Initialise la stratégie"
  (copy-grid grid)
  (init-coups-possibles)
  (init-cases-coups-possibles))  


(defun main-standalone ()
"Lance la stratégie"
  (let ((longueur (length +CASES-COUPS-POSSIBLES+))
	(coord NIL)
	(val NIL))

    (if (<= longueur 0)
	NIL
	(progn
	  (do ((i 0 (1+ i))) ;;si il existe une cases avec une seule possibilité
	      ((>= i longueur))
	    (let ((x (first (nth i +CASES-COUPS-POSSIBLES+)))
		  (y (second (nth i +CASES-COUPS-POSSIBLES+))))
	      (if (eq (length (aref +COUPS-POSSIBLES+ x y)) 1)
		  (progn
		    (setf coord (list x y))
		    (setf val (first (aref +COUPS-POSSIBLES+ x y)))))))

	  (if (equal val NIL) ;;si il existe une case dans ligne étant la seule à pouvoir contenir une valeur
	      (do ((j 0 (1+ j))) ;;parcours ordonnée
		  ((>= j +LONG-SIZE+)
		   (not (equal val NIL)))
		(do ((i 0 (1+ i))) ;;parcours abscisse
		    ((>= i +LONG-SIZE+)
		     (not (equal val NIL)))
		  (let ((diff (aref +COUPS-POSSIBLES+ i j))) ;;2eme parcours abscisse
		    (do ((k (1+ i) (1+ k))) ;;on supprime si 2ème occurence ailleurs
			((>= k +LONG-SIZE+)
			 (not (equal val NIL)))
		      (setf diff (set-difference diff (aref +COUPS-POSSIBLES+ k j))))
		    (if (not (endp diff))
		      (progn
			(setf coord (list i j))
			(setf val (first diff))))))))

	  (if (equal val NIL) ;;si il existe une case dans colonne étant la seule à pouvoir contenir une valeur
	      (do ((j 0 (1+ j))) ;;parcours abscisse
		  ((>= j +LONG-SIZE+)
		   (not (equal val NIL)))
		(do ((i 0 (1+ i))) ;;parcours ordonnée
		    ((>= i +LONG-SIZE+)
		     (not (equal val NIL)))
		  (let ((diff (aref +COUPS-POSSIBLES+ j i))) ;;2eme parcours ordonnée
		    (do ((k (1+ i) (1+ k))) ;;on supprime si 2ème occurence ailleurs
			((>= k +LONG-SIZE+))
		      (setf diff (set-difference diff (aref +COUPS-POSSIBLES+ j k))))
		    (if (not (endp diff))
		      (progn
			(setf coord (list j i))
			(setf val (first diff))))))))

	  (if (equal val NIL);;s'il existe une case dans le carré étant la seule à pouvoir contenir une valeur
	      (do ((i 0 (1+ i)));;parcours abscisse
		  ((>= i +LONG-SIZE+)
		   (not (equal val NIL)))
		(do ((j 0 (1+ j)));;parcours ordonnee
		    ((>= j +LONG-SIZE+)
		     (not (equal val NIL)))
		  (let* ((diff (aref +COUPS-POSSIBLES+ i j))
			 (res-de-I (multiple-value-bind (q r) (floor i +SIZE+) (list q r)))
			 (res-de-J (multiple-value-bind (q r) (floor j +SIZE+) (list q r)))
			 (quotient-de-I (first res-de-I))
			 (quotient-de-J (first res-de-J))
			 (reste-de-I (second res-de-I))
			 (reste-de-J (second res-de-J)))
		    (do ((k reste-de-I (1+ k)))
			((>= k +SIZE+))
		      (do ((l 0 (1+ l)))
			  ((>= l +SIZE+))
			(if (not (and (eq k reste-de-I) (<= l reste-de-J)))
			    (setf diff (set-difference diff (aref +COUPS-POSSIBLES+ 
								  (+ quotient-de-I k) 
								  (+ quotient-de-J l)))))))
		    (if (not (endp diff))
			(progn 
			  (setf coord (list i j))
			  (setf val (first diff))))))))
	  
	  ;;On met les coups possibles de la case à NIL
	  (setf (aref +COUPS-POSSIBLES+ (first coord) (second coord)) '() )
	  (remove coord +CASES-COUPS-POSSIBLES+ :test #'equal)
			

	  ;;On supprime les coups possibles val dans la ligne
	  (do ((i 0 (1+ i))) 
	      ((>= i +LONG-SIZE+))
	    (setf (aref +COUPS-POSSIBLES+
			i
			(second coord))
		  (remove val (aref +COUPS-POSSIBLES+
				    i
				    (second coord)) :test #'equal))
	    (if (and (endp (aref +COUPS-POSSIBLES+ 
				 i 
				 (second coord)))
		     (= (aref +GRID-ACCESS+ i (second coord)) 0))
		(setf +CASES-COUPS-POSSIBLES+
		      (remove (list i (second coord))
			      +CASES-COUPS-POSSIBLES+ 
			      :test #'equal))))
	  
	  ;;On supprime les coups possibles val dans la colonne
	  (do ((j 0 (1+ j)));;parcours ordonnee
	      ((>= j +LONG-SIZE+))
	    (setf (aref +COUPS-POSSIBLES+
			(first coord)
			j)
		  (remove val (aref +COUPS-POSSIBLES+
				    (first coord)
				    j) :test #'equal))
	    (if (and (endp (aref +COUPS-POSSIBLES+ 
			    (first coord) 
			    j))
		     (= (aref +GRID-ACCESS+ (first coord) j) 0))
		(setf +CASES-COUPS-POSSIBLES+
		      (remove (list (first coord) j)
			      +CASES-COUPS-POSSIBLES+ 
			      :test #'equal))))

	  ;;On supprime les coups possible val dans le carré
	  (do ((i (- (first coord) 
		     (mod (first coord) +SIZE+)) 
		  (1+ i)))
	      ((>= i (+ (mod (first coord) +SIZE+) +SIZE+)))
	    (do ((j (- (second coord) 
		       (mod (second coord) +SIZE+)) 
		    (1+ j)))
		((>= j (+ (mod (second coord) +SIZE+) +SIZE+)))
	      (setf (aref +COUPS-POSSIBLES+ i j)
		    (remove val (aref +COUPS-POSSIBLES+ i j) 
			    :test #'equal))
	      (if (and (endp (aref +COUPS-POSSIBLES+ 
				   i 
				   j))
		       (= (aref +GRID-ACCESS+ i j) 0))
		    (setf +CASES-COUPS-POSSIBLES+
			  (remove (list i j)
				  +CASES-COUPS-POSSIBLES+ 
				  :test #'equal)))))
	  (setf *cmp* (1+ *cmp*))
	  (print *cmp*)
	  (values (first coord) (second coord) val)))))

(defun test-main()
  (let ((ret (multiple-value-bind (x y val) (main-standalone) (list x y val))))
    (do ()
	((eq (first ret) NIL))
      (setf (aref +GRID+ (first ret) (second ret))
	    (third ret))
      (setf ret (multiple-value-bind (x y val) 
		    (main-standalone) 
		  (list x y val)))))
  (print "fin"))
      



  
  
    
  
  
      
      
  
