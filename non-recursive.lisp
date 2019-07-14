;;;; Non-recursive implementation of the package-merge algorithm.

(in-package :pm)

(defun pm-nonrecur (faces weights X)
  "Non-recursive version of package-merge.
I: faces: Array of faces. Not ordered, but must correspond with 'weights'.
   weights: Array of weights. Must correspond with the order in 'faces'.
   X: Vector of i values in decreasing order. The number X represents is a 
      decomposition in powers of 2, being 2^i.
      eg. #(4 2 -1 -3) for X = 2^4 + 2^2 + 2^-1 + 2^-3." 
  (let* ((ncoins (length faces))
	 ;; Set of lists Ld. The hash is d for each.
	 (Lds (make-hash-table :size ncoins))
	 ;; Set of coins.
	 (I (make-array ncoins :fill-pointer ncoins
			       :element-type 'coin))
	 (X-cop (make-array (length X) :fill-pointer (length X)
				       :element-type 'fixnum))
	 ;; Face value for coins: 2^d.
	 (d nil)
	 (dbuf 0)
	 (curld nil)
	 ;; List of existing d for coins.
	 (ds (make-array 0 :fill-pointer 0 :element-type 'fixnum))
	 )
    ;; Copy X
    (loop for i from 0 below (length X) do
      (setf (aref X-cop i) (aref X i)))
    ;; Create set of coins.
    (loop for face across faces
	  for weight across weights
	  for id from 0 do
	    (setf (aref I id) (make-coin :id id :face face :weight weight)))
    (setf I (sort I #'coin-order))
    ;; Fill the hash table with sets Ld, we can simply pop elements from I,
    ;; they are in the exact reverse order we need.
    (loop while (> (length I) 0) do
      (setf dbuf (coin-face (aref I (1- (length I)))))
      (when (or (not d) (/= d dbuf))
	(setf d dbuf)
	(vector-push-extend d ds)
	(setf (gethash d Lds) (make-array 0 :fill-pointer 0
					    :element-type 'coin))
	(setf curld (gethash d Lds)))
      (vector-push-extend (vector-pop I) curld))
    ;; Main algorithm loop.
    
    ))
