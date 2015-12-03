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
	     (print "Impossible de mettre cette valeur dans cette case."))
	 (print "Cette case n'est pas accessible"))))

(defun test-ligne-colonne-carre(largeur hauteur valeur)
"Lance les trois test"
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

    

(defun jeu-de-test ()
  (let ((longueur (1- (* +SIZE+ +SIZE+))))
    (assert (init-all +SIZE+))
    (assert (set-tile 'b 5 6))
    (assert (set-tile 'f 6 9))
    (assert (set-tile 'a 1 1))
    (assert (not (test-ligne longueur 0 1)))
    (assert (test-ligne longueur 3 8))
    (assert (not (test-colonne 0 longueur 1)))
    (assert (test-colonne 0 longueur 2))
    (assert (test-carre 8 8 4))
    (assert (not (test-carre 5 5 9))))
  T)

(defun jeu-de-test2()
  (copy-grid *grid-de-test*)
  (print-grid))
  
    
  
  
      
      
  
