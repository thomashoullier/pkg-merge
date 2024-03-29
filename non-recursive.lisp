;;;; Non-recursive implementation of the package-merge algorithm.

(in-package :pm)

(defun pm-nonrec (faces weights X)
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
	 (d)
	 (dbuf 0)
	 (curld)
	 (minwidth 0)
	 ;; List of existing d for coins.
	 (ds (make-array 0 :fill-pointer 0 :element-type 'fixnum))
	 ;; Optimal solution set of coins.
	 (S (make-array 0 :fill-pointer 0 :element-type 'coin))
	 ;; Ids of the solution set of coins.
	 (sol-coins (make-array 0 :fill-pointer 0 :element-type 'fixnum)))
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
    (loop while (> (length X-cop) 0)
	  for iloop from 0 do
      (if (> (length ds) 0)
	  (setf d (aref ds (1- (length ds))))
	  (return-from pm-nonrec nil))
      (setf curld (gethash d Lds))
      (setf minwidth (aref X-cop (1- (length X-cop))))
      ;; The smallest coin cannot pay for the smallest term in X.
      (when (> d minwidth) (return-from pm-nonrec nil))
      (when (= d minwidth)
	;; Use the smallest coin in Ld to pay for a part of X.
	(vector-push-extend (vector-pop curld) S)
	(vector-pop X-cop))
      ;; Perform PACKAGE and MERGE.
      ;; TODO: This part is sub-optimal at the moment.
      (let ((coin1) (coin2)
	    (nextld (gethash (1+ d) Lds))
	    (sortp))
	;; Remove the current d from the available list
	(vector-pop ds)
	;; If packages will be made and nextld is not declare then do it
	(when (and (not nextld) (> (length curld) 1))
	  (setf (gethash (1+ d) Lds)
		(make-array 0 :fill-pointer 0 :element-type 'coin))
	  (setf nextld (gethash (1+ d) Lds))
	  (vector-push-extend (1+ d) ds))
	;; Check whether a sort will be needed.
	(when (> (length curld) 1) (setf sortp T))
	;; Make all the packages and push into next set.
	(loop while (> (length curld) 1) do
	  (psetf coin1 (vector-pop curld)
		 coin2 (vector-pop curld))
	  (vector-push-extend
	   (make-coin :face (1+ d)
		      :weight (+ (coin-weight coin1)
				 (coin-weight coin2))
		      :left-coin coin1
		      :right-coin coin2) nextld))
	;; Discard Ld
	(remhash d Lds)
	;; Finally sort the new set if needed.
	(when (and nextld sortp)
	  (setf (gethash (1+ d) Lds)
		(sort (gethash (1+ d) Lds) 
		      (lambda (coini1 coini2) (> (coin-weight coini1)
						 (coin-weight coini2))))))))
    ;; S is the solution set of coins/packages. It's a forest, we must get the
    ;; ids of the leaves of all the trees.
    (loop for coinpack across S do
	  (leafcoins coinpack sol-coins))
    sol-coins))
