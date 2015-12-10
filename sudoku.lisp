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
      

(defun set-tile(largeur hauteur valeur)
  "Attribue à la case de coordonnée (largeur, hauteur) de +GRID+ valeur, si valeur n'est pas déjà dans la ligne, colonne ou carré"
  (let ((l (1- (getHash largeur +TAB-HASH+)))
	(h (1- hauteur)))
    (if (eq (aref +GRID-ACCESS+ h l) 0)
	 (if (test-ligne-colonne-carre l h valeur)
	     (if (or (or (< valeur 1) (> valeur +LONG-SIZE+))
		     (> h +LONG-SIZE+)
		     (> l +LONG-SIZE+))
		 NIL
		 (setf (aref +GRID+ h l) valeur))
	     (format t "Impossible de mettre cette valeur dans cette case. ~%"))
	 (format t "Cette case n'est pas accessible. ~%"))))

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
	
    
(defun print-grid()
  "Affiche la grille"
  (format t "~a" "  ")
  (do ((i 0 (1+ i)))
      ((>= i +LONG-SIZE+))
    (print-grid-separateur-colonne i)
    (format t "~a" (nth i +ALPHABET+))
    (format t "~a" " "))
  (format t "|~%" )
  (print-grid-separateur-ligne 0)
 
  (do ((i 1 (1+ i)))
      ((> i +LONG-SIZE+))
    (print-grid-ligne i)
    (print-grid-separateur-ligne i)))

(defun print-grid-ligne(h)
"Utilisée dans print-grid, affiche une ligne."
  (format t "~a" h)
  (format t "~a" " ")
  (do ((i 0 (1+ i)))
      ((>= i +LONG-SIZE+))
    (print-grid-separateur-colonne i)
    (if (not (eq (aref +GRID+ (1- h) i) 0))
	(format t "~a" (aref +GRID+ (1- h) i))
	(format t "~a" "."))
    (format t "~a" " "))
  (format t "|~%"))
    

(defun print-grid-separateur-colonne(i)
"Utilisée dans print-grid et print-grid-ligne, affiche le séparateur de colonne."
  (if (eq (mod i +SIZE+) 0)
      (format t "~a" "| ")))

(defun print-grid-separateur-ligne(i)
"Utilisée dans print-grid, affiche le séparateur de ligne."
  (if (eq (mod i +SIZE+) 0)
      (progn
	(do ((i 0 (1+ i)))
	    ((>= i (+ +LONG-SIZE+ 4 (1- +SIZE+))))
	  (format t "- "))
	(format t " ~%"))))

(defun input()
"Demande à l'utilisateur les coordonnées et la valeur qu'il souhaite entrer dans celles-ci, par entrée clavier."
  (let ((l (list)))
    (format t "Colonne - Ligne - Valeur ~%")
    (let ((col (read))
	  (lig (read))
	  (val (read)))
      (if (find col (subseq +ALPHABET+ 0 +LONG-SIZE+))
	  (progn 
	    (setf l (cons col l))
	    (if (and (>= lig 0) (<= lig +LONG-SIZE+))
		(progn
		  (setf l (append l (list lig)))
		  (if (and (>= val 0) (<= val +LONG-SIZE+))
		      (setf l (append l (list val)))
		      (progn 
			(format t "Erreur : Valeur invalide. Veuillez tout recommencer. ~%")
			(input))))
		(progn
		  (format t "Erreur : Ligne invalide. Veuillez tout recommencer. ~%")
		  (input))))
	  (progn
	    (format t "Erreur : Colonne invalide. Veuillez tout recommencer. ~%")
	    (input))))
    l))
		  
	  
(defun isFinish()
"Retourne T si le jeu est fini (toutes les cases sont remplies), NIL sinon."
  (do ((i 0 (+ 1 i)))
      ((>= i +LONG-SIZE+))
    (do ((j 0 (+ 1 j)))
	((>= j +LONG-SIZE+))
      (if (eq (aref +GRID+ i j) 0)
	  (return-from isFinish) NIL)))
  T)


(defun play()
"Lance le jeu sur +GRID+."
  (do ()
      ((isFinish))
    (print-grid)
    (let ((l (input)))
      (set-tile (car l) (second l) (third l))))
  (format t "Partie finie ! ~%"))

(defun sudoku(grid)
"Initialise +GRID+ en copiant grid dedans, et lance le jeu."
  (copy-grid grid)
  (play))
      
      
  
;;; test 

(defun jeu-de-test ()
  (let ((longueur (1- +LONG-SIZE+)))
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



  
  
    
  
  
      
      
  
