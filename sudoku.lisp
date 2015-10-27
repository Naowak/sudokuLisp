(defparameter +tabHash+
  (make-hash-table :test #'eq))

(defparameter +grid+
  (creer-grid 3))

(defun creer-grid (size)
  (make-array (list (* size size) (* size size))))

(defun creer-hash(size)
  (if (> size 26)
      NIL
      (creer-hash-term 
       size 
       (subseq 
	'(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z) 
	0 size))))

(defun creer-hash-term(size list)
  (setf (gethash (car (last list)) +tabHash+) size)
  (if (= 1 size)
      T
      (creer-hash-term (1- size) (butlast list))))
      
      

(defun set-tile(grid hauteur largeur valeur)
  (let ((taille (car (array-dimensions grid)))
	(l (1- (getHash largeur +tabHash+)))
	(h (1- hauteur)))
  (if (or (or (< valeur 1) (> valeur taille))
	   (> h taille)
	   (> l taille))
      NIL
      (setf (aref grid h l) valeur)))) 
      
      
      
  
