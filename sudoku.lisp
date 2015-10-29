(defconstant +SIZE+ 3)

(defparameter +tabHash+
  (make-hash-table :test #'eq))

(defparameter +grid+
  (creer-grid +SIZE+))

(defparameter +grid-access+
  (creer-grid +SIZE+))

(defun creer-grid (size)
  (make-array (list (* size size) (* size size))))

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
  (initialiser-hash (* size size))
  (if (not (= size +SIZE+))
      (progn 
	(setf +grid+ (creer-grid size))
	(setf +grid-access+ (creer-grid size)))
      T))
      

(defun set-tile(largeur hauteur valeur)
  (let ((taille (car (array-dimensions +grid+)))
	(l (1- (getHash largeur +tabHash+)))
	(h (1- hauteur)))
  (if (or (or (< valeur 1) (> valeur taille))
	   (> h taille)
	   (> l taille))
      NIL
      (setf (aref +grid+ h l) valeur)))) 


(defun test-ligne(n h valeur)
  (if (< n 0)
      T
      (if (= (aref +grid+ h n) valeur)
	  NIL
	  (test-ligne (1- n) h valeur))))

(defun test-colonne(l n valeur)
  (if (< n 0)
      T
      (if (= (aref +grid+ n l) valeur)
	  NIL
	  (test-colonne l (1- n) valeur))))


(defun test-carre(l h valeur)
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
    
  
  
      
      
  
