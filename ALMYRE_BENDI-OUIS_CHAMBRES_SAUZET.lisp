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

(defun creer-grid ()
"Créé une grille de taille (size*size)*(size*size)"
  (make-array (list +LONG-SIZE+ +LONG-SIZE+)))

;;Grille du sudoku
(defparameter +GRID+
  (creer-grid))

;;Grille qui définit quelles cases sont modifiables
(defparameter +GRID-ACCESS+
  (creer-grid))

;;grilles de sudoku données pour tester la stratégie

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

(defparameter *grid-4*
  (make-array '(9 9) :initial-contents
	      '((4 3 2 0 8 5 0 6 7)
		(5 0 9 2 0 6 0 4 0)
		(0 0 0 0 0 0 5 0 2)
		(8 5 4 0 0 0 2 0 0)
		(0 7 3 8 0 2 9 0 0)
		(9 0 0 5 0 7 8 1 4)
		(0 0 0 1 5 3 0 2 9)
		(2 9 5 7 6 0 4 3 0)
		(3 6 1 4 2 9 0 0 0))))

(defparameter *grid-5*
  (make-array '(9 9) :initial-contents
	      '((4 1 0 0 5 2 0 3 0)
		(6 3 0 7 8 0 4 0 0)
		(0 8 5 0 0 4 0 0 7)
		(0 0 0 5 0 0 2 0 0)
		(0 4 1 2 7 0 0 8 0)
		(0 0 0 8 1 0 6 0 0)
		(0 7 0 4 0 0 3 9 8)
		(3 6 8 1 9 0 0 0 2)
		(9 0 0 3 2 0 0 1 0))))

(defparameter *grid-6*
  (make-array '(9 9) :initial-contents
	      '((0 0 6 0 0 2 8 0 1)
		(0 3 2 1 9 0 7 0 0)
		(1 0 0 0 0 4 3 9 2)
		(7 0 1 0 0 0 0 0 0)
		(0 5 0 0 2 0 0 0 8)
		(9 0 0 4 3 0 1 5 6)
		(2 1 4 7 6 0 0 0 0)
		(8 0 5 2 0 0 6 0 0)
		(3 0 0 0 0 5 2 0 7))))

(defparameter *grid-7*
  (make-array '(9 9) :initial-contents
	      '((0 2 0 0 0 0 0 0 0)
		(6 0 0 5 1 0 0 0 7)
		(7 0 9 0 0 0 0 0 0)
		(5 0 4 0 8 0 1 0 3)
		(0 1 0 0 3 2 0 4 0)
		(2 0 3 1 0 5 6 0 0)
		(0 0 0 3 5 1 0 8 6)
		(1 0 0 4 7 0 0 3 0)
		(3 0 0 6 0 0 0 5 0))))



(defparameter *grid-8*
  (make-array '(9 9) :initial-contents
	      '((0 4 0 0 0 0 3 0 0)
		(0 0 6 8 2 0 0 4 7)
		(0 3 0 4 0 5 0 0 9)
		(0 8 4 0 0 7 9 0 0)
		(3 2 0 0 8 0 0 5 1)
		(0 0 1 3 0 0 4 6 0)
		(4 0 0 9 0 8 0 7 0)
		(7 1 0 0 5 6 8 0 0)
		(0 0 8 0 0 0 0 3 0))))


(defparameter *grid-9*
  (make-array '(9 9) :initial-contents
	      '((0 0 1 0 5 0 0 9 2)
		(3 0 0 8 0 0 0 0 0)
		(0 4 2 6 0 0 0 1 5)
		(1 6 0 0 8 7 0 0 0)
		(0 9 0 1 3 6 0 8 0)
		(0 0 0 9 4 0 0 6 1)
		(7 1 0 0 0 3 2 5 0)
		(0 0 0 0 0 8 0 0 7)
		(4 2 0 0 1 0 6 0 0))))



(defparameter *grid-10*
  (make-array '(9 9) :initial-contents
	      '((1 2 0 0 0 5 6 9 0)
		(9 6 0 1 0 0 7 0 0)
		(0 0 5 0 0 9 0 0 0)
		(6 8 3 0 0 0 0 0 7)
		(2 0 1 0 0 0 5 0 3)
		(5 0 0 0 0 0 9 1 6)
		(0 0 0 8 0 0 4 0 0)
		(0 0 6 0 0 4 0 5 2)
		(0 4 9 5 0 0 0 6 8))))



(defparameter *grid-11*
  (make-array '(9 9) :initial-contents
	      '((0 1 0 0 0 0 0 0 0)
		(0 0 0 3 0 0 0 4 2)
		(0 0 0 2 9 0 7 6 0)
		(0 8 0 0 6 0 1 9 0)
		(1 0 0 7 0 8 0 0 4)
		(0 7 6 0 1 0 0 3 0)
		(0 9 3 0 8 4 0 0 0)
		(6 4 0 0 0 7 0 0 0)
		(0 0 0 0 0 0 0 1 0))))


(defparameter *grid-12*
  (make-array '(9 9) :initial-contents
	      '((0 0 1 6 5 0 0 0 0)
		(4 6 0 0 0 3 0 0 1)
		(0 7 0 0 0 0 0 0 2)
		(0 3 4 0 0 0 0 0 9)
		(1 0 0 8 3 9 0 0 4)
		(9 0 0 0 0 0 6 3 0)
		(6 0 0 0 0 0 0 2 0)
		(7 0 0 4 0 0 0 9 8)
		(0 0 0 0 1 5 7 0 0))))




(defparameter *grid-13*
  (make-array '(9 9) :initial-contents
	      '((0 0 0 0 0 3 0 0 2)
		(7 0 0 0 8 0 0 1 5)
		(8 6 3 0 1 0 0 0 0)
		(0 0 0 0 0 6 5 4 0)
		(0 0 6 1 0 5 7 0 0)
		(0 3 5 7 0 0 0 0 0)
		(0 0 0 0 5 0 4 6 1)
		(4 5 0 0 6 0 0 0 7)
		(6 0 0 9 0 0 0 0 0))))




(defparameter *grid-14*
  (make-array '(9 9) :initial-contents
	      '((0 0 4 5 1 0 0 0 3)
		(0 0 0 0 0 3 0 8 0)
		(0 6 0 0 8 4 2 0 0)
		(4 0 0 7 0 0 0 0 0)
		(5 0 7 0 0 0 8 0 6)
		(0 0 0 0 0 8 0 0 4)
		(0 0 2 8 9 0 0 3 0)
		(0 9 0 1 0 0 0 0 0)
		(3 0 0 0 4 5 9 0 0))))



(defparameter *grid-15*
  (make-array '(9 9) :initial-contents
	      '((7 6 0 4 0 0 0 0 0)
		(0 3 0 0 2 0 0 0 0)
		(4 0 0 0 1 0 5 6 0)
		(0 0 0 2 7 0 3 1 0)
		(5 0 0 0 0 0 0 0 2)
		(0 1 8 0 9 3 0 0 0)
		(0 2 7 0 8 0 0 0 1)
		(0 0 0 0 3 0 0 2 0)
		(0 0 0 0 0 2 0 3 4))))



(defparameter *grid-16*
  (make-array '(9 9) :initial-contents
	      '((1 0 0 0 0 4 0 0 5)
		(0 0 0 9 5 0 0 8 0)
		(0 0 0 0 0 3 0 9 0)
		(0 0 5 0 0 2 0 0 4)
		(0 0 1 0 6 0 7 0 0)
		(7 0 0 3 0 0 2 0 0)
		(0 6 0 5 0 0 0 0 0)
		(0 8 0 0 1 6 0 0 0)
		(5 0 0 2 0 0 0 0 7))))



(defparameter *grid-17*
  (make-array '(9 9) :initial-contents
	      '((4 6 0 7 2 0 8 5 3)
		(0 0 7 4 3 9 2 0 6)
		(0 0 0 8 0 6 9 4 0)
		(2 7 0 9 1 0 3 6 4)
		(1 0 3 6 8 0 5 7 2)
		(0 4 5 2 7 3 1 8 9)
		(8 1 2 3 6 7 0 9 0)
		(9 5 0 1 0 0 7 3 8)
		(7 3 4 0 9 8 0 2 1))))



(defparameter *grid-18*
  (make-array '(9 9) :initial-contents
	      '((6 7 0 0 8 2 1 0 4)
		(0 8 2 3 1 4 0 5 0)
		(0 4 1 5 7 6 0 2 0)
		(4 0 9 8 0 0 0 6 7)
		(0 3 6 4 5 0 9 0 2)
		(2 0 8 7 6 0 0 4 1)
		(5 9 0 0 0 7 0 1 8)
		(0 0 0 2 9 0 4 7 0)
		(8 2 0 0 0 5 6 9 3))))



(defparameter *grid-19*
  (make-array '(9 9) :initial-contents
	      '((0 1 4 0 6 0 0 0 7)
		(8 0 0 0 4 0 1 0 0)
		(0 0 6 0 1 0 0 8 5)
		(0 8 3 0 0 0 0 5 0)
		(4 0 0 6 5 0 0 0 8)
		(5 7 0 0 0 2 0 0 0)
		(3 0 5 8 2 1 0 7 6)
		(0 0 7 9 3 0 0 1 4)
		(0 0 8 0 0 6 5 3 0))))



(defparameter *grid-20*
  (make-array '(9 9) :initial-contents
	      '((0 0 0 2 8 1 0 5 3)
		(0 0 0 0 5 3 0 9 0)
		(5 0 3 4 9 0 0 0 0)
		(1 7 0 0 3 5 2 6 0)
		(0 0 9 0 2 4 0 3 5)
		(3 0 0 0 6 0 9 4 0)
		(0 5 0 6 0 8 0 0 0)
		(8 9 6 0 4 0 5 1 7)
		(7 3 0 5 1 9 6 0 4))))



(defparameter *grid-21*
  (make-array '(9 9) :initial-contents
	      '((4 6 2 0 9 3 7 0 5)
		(0 7 0 2 0 4 0 9 0)
		(5 9 0 7 0 0 4 0 1)
		(0 5 0 8 1 7 0 4 9)
		(7 0 1 0 4 0 0 6 3)
		(0 0 0 6 3 5 8 0 0)
		(9 2 4 0 7 0 0 3 0)
		(0 3 7 4 8 0 0 5 2)
		(0 0 0 0 2 0 6 0 4))))



(defparameter *grid-22*
  (make-array '(9 9) :initial-contents
	      '((0 0 9 0 2 8 1 5 6)
		(0 0 2 9 6 0 4 0 0)
		(0 6 0 7 0 3 0 0 0)
		(0 2 0 5 4 0 0 0 7)
		(0 0 5 6 3 0 2 0 1)
		(4 0 1 0 0 2 3 6 5)
		(2 4 7 0 0 0 0 0 0)
		(9 5 0 2 8 7 6 0 0)
		(0 1 8 3 5 4 7 9 2))))



(defparameter *grid-23*
  (make-array '(9 9) :initial-contents
	      '((0 7 0 0 0 6 0 2 0)
		(0 9 8 4 0 0 7 1 5)
		(0 3 0 9 0 0 0 6 4)
		(0 0 6 3 9 1 0 4 8)
		(1 0 0 6 0 8 5 0 7)
		(3 8 4 2 0 7 6 9 0)
		(8 4 2 7 0 0 1 0 9)
		(5 0 0 8 2 0 4 0 6)
		(9 6 7 0 1 0 0 8 2))))



(defparameter *grid-24*
  (make-array '(9 9) :initial-contents
	      '((7 8 0 0 0 4 0 0 0)
		(0 2 0 0 8 3 0 6 0)
		(6 4 5 9 0 7 8 3 0)
		(0 3 2 0 4 6 7 0 9)
		(8 6 0 0 0 9 3 0 1)
		(0 5 9 0 7 0 2 0 6)
		(2 9 8 0 0 0 0 0 3)
		(0 7 0 1 3 2 0 9 8)
		(0 1 6 7 9 8 5 0 4))))



(defparameter *grid-25*
  (make-array '(9 9) :initial-contents
	      '((0 5 4 1 0 0 0 7 8)
		(1 0 0 3 6 0 4 0 5)
		(8 0 0 5 0 0 0 9 3)
		(4 0 5 0 3 2 0 6 1)
		(6 3 8 0 1 0 2 5 9)
		(0 0 0 6 8 5 7 3 0)
		(5 8 9 2 7 0 0 4 0)
		(7 4 6 0 5 3 9 1 2)
		(0 0 3 4 9 0 0 0 7))))









;;tableau contenant tous les coups possibles pour chaque case
(defparameter +COUPS-POSSIBLES+
  (creer-grid))

;;liste contenant toutes les cases où un coup est possible
(defparameter +CASES-COUPS-POSSIBLES+
  (list))

(defun reset-all()
  (setf +CASES-COUPS-POSSIBLES+ (list))
  (setf +COUPS-POSSIBLES+ (creer-grid))
  (setf +GRID+ (creer-grid))
  (setf +GRID-ACCESS+ (creer-grid))
  T)

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
  (reset-all)
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

	  (if (equal val NIL) ;;si il existe une case dans une ligne étant la seule à pouvoir contenir une valeur
	      (do ((j 0 (1+ j))) ;;parcours ordonnée
		  ((>= j +LONG-SIZE+)
		   (not (equal val NIL)))
		(do ((i 0 (1+ i))) ;;parcours abscisse
		    ((>= i (1- +LONG-SIZE+))
		     (not (equal val NIL)))
		  (let ((diff (aref +COUPS-POSSIBLES+ i j))) ;;2eme parcours abscisse
		    (do ((k (1+ i) (1+ k))) ;;on supprime si 2ème occurence ailleurs
			((>= k +LONG-SIZE+))
		      (setf diff (set-difference diff (aref +COUPS-POSSIBLES+ k j))))
		    (if (not (endp diff))
		      (progn
			(setf coord (list i j))
			(setf val (first diff))))))
		(let ((diff (aref +COUPS-POSSIBLES+ (1- +LONG-SIZE+) j)))
		  (do ((i (- +LONG-SIZE+ 2) (1- i)))
		      ((< i 0))
		    (setf diff (set-difference diff (aref +COUPS-POSSIBLES+ i j))))
		  (if (not (endp diff))
		      (progn
			(setf coord (list (1- +LONG-SIZE+) j))
			(setf val (first diff)))))))
		  

	  (if (equal val NIL) ;;si il existe une case dans colonne étant la seule à pouvoir contenir une valeur
	      (do ((j 0 (1+ j))) ;;parcours abscisse
		  ((>= j +LONG-SIZE+)
		   (not (equal val NIL)))
		(do ((i 0 (1+ i))) ;;parcours ordonnée
		    ((>= i (1- +LONG-SIZE+))
		     (not (equal val NIL)))
		  (let ((diff (aref +COUPS-POSSIBLES+ j i))) ;;2eme parcours ordonnée
		    (do ((k (1+ i) (1+ k))) ;;on supprime si 2ème occurence ailleurs
			((>= k +LONG-SIZE+))
		      (setf diff (set-difference diff (aref +COUPS-POSSIBLES+ j k))))
		    (if (not (endp diff))
		      (progn
			(setf coord (list j i))
			(setf val (first diff))))))
		(let ((diff (aref +COUPS-POSSIBLES+ j (1- +LONG-SIZE+))))
		  (do ((i (- +LONG-SIZE+ 2) (1- i)))
		      ((< i 0))
		    (setf diff (set-difference diff (aref +COUPS-POSSIBLES+ j i))))
		  (if (not (endp diff))
		      (progn
			(setf coord (list j (1- +LONG-SIZE+)))
			(setf val (first diff)))))))

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
			  (setf val (first diff)))))))
	      
	      (do ((i 0 (1+ i)))
		  ((>= i +SIZE+))
		(do ((j 0 (1+ j)))
		    ((>= j +SIZE+))
		  (let* ((x (* +SIZE+ i))
			 (y (* +SIZE+ j))
			 (xmax (+ x (1- +SIZE+)))
			 (ymax (+ y (1- +SIZE+)))
			 (diff (aref +COUPS-POSSIBLES+ xmax ymax)))
		    (do ((k (1- +SIZE+) (1- k)))
			((< k 0))
		      (do ((l (1- +SIZE+) (1- l)))
			  ((< l 0))
			(if (not (and (eq (+ x k) xmax)
				      (eq (+ y l) ymax)))
			    (setf diff (set-difference diff (aref +COUPS-POSSIBLES+
								  (+ x k)
								  (+ y l)))))))
		    (if (not (endp diff))
			(progn 
			  (setf coord (list xmax ymax))
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
	      ((>= i (* (1+ (floor (first coord) +SIZE+)) +SIZE+) +SIZE+))
	    (do ((j (- (second coord) 
		       (mod (second coord) +SIZE+)) 
		    (1+ j)))
		((>= j (* (1+ (floor (second coord) +SIZE+)) +SIZE+) +SIZE+))
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
  (print +grid+))

(defun isFinish()
"Retourne T si le jeu est fini (toutes les cases sont remplies), NIL sinon."
  (do ((i 0 (+ 1 i)))
      ((>= i +LONG-SIZE+))
    (do ((j 0 (+ 1 j)))
	((>= j +LONG-SIZE+))
      (if (eq (aref +GRID+ i j) 0)
	  (return-from isFinish) NIL)))
  T)

(defun test-all-grid()
  (let ((i 0))
    (format t "~%~%Grille 1 : ")
    (init-standalone *grid-1*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))
    
    (format t "~%~%Grille 2 : ")
    (init-standalone *grid-2*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 3 :")
    (init-standalone *grid-3*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 4 : ")
    (init-standalone *grid-4*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 5 : ")
    (init-standalone *grid-5*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 6 :")
    (init-standalone *grid-6*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 7 : ")
    (init-standalone *grid-7*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 8 : ")
    (init-standalone *grid-8*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 9 :")
    (init-standalone *grid-9*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 10 : ")
    (init-standalone *grid-10*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 11 : ")
    (init-standalone *grid-11*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 12 :")
    (init-standalone *grid-12*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 13 : ")
    (init-standalone *grid-13*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 14 : ")
    (init-standalone *grid-14*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 15 :")
    (init-standalone *grid-15*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 16 : ")
    (init-standalone *grid-16*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 17 : ")
    (init-standalone *grid-17*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 18 :")
    (init-standalone *grid-18*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 19 : ")
    (init-standalone *grid-19*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 20 : ")
    (init-standalone *grid-20*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 21 :")
    (init-standalone *grid-21*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 22 : ")
    (init-standalone *grid-22*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 23 : ")
    (init-standalone *grid-23*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 24 :")
    (init-standalone *grid-24*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))

    (format t "~%~%Grille 25 :")
    (init-standalone *grid-25*)
    (test-main)
    (if (isFinish)
	(progn
	  (format t "Grille Complète !~%")
	  (setf i (1+ i))))
    (format t "~%")
    (format t "~$" i)
    (format t "/25 grilles complètes ! ~%")))
  
      



  
  
    
  
  
      
      
  
