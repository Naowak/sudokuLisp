(defconstant +SIZE+ 3)

(defparameter +tabHash+
  (make-hash-table :test #'eq))

(defun creer-grid (size)
  (make-array (list (* size size) (* size size))))

(defparameter +grid+
  (creer-grid +SIZE+))

(defparameter +grid-access+
  (creer-grid +SIZE+))

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
  (initialiser-hash size)
  (if (not (= size +SIZE+))
      (progn 
	(setf +grid+ (creer-grid size))
	(setf +grid-access+ (creer-grid size)))
      T))
      

(defun set-tile(hauteur largeur valeur)
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

(defun test-colonne(n l valeur)
  (if (< n 0)
      T
      (if (= (aref +grid+ n l) valeur)
	  NIL
	  (test-colonne l (1- n) valeur))))

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

(defun test-carre(l h valeur)
  (let ((x (floor (/ l +SIZE+)))
	(y (floor (/ h +SIZE+))))
    (test-carre-recX (1- +SIZE+) x y valeur)))
    
  
  
      
      
  
