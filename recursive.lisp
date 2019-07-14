;;;; Recursive algorithm for package-merge.
;;;; This is the general case of package-merge, not specialized for
;;;; length-limited Huffman coding.

(in-package :pm)

(defun pm-rec (faces weights X)
  "Recursive version of package-merge.
I: faces: Array of faces. Not ordered, but must correspond with 'weights'.
   weights: Array of weights. Must correspond with the order in 'faces'.
   X: Vector of i values in decreasing order. The number X represents is a 
      decomposition in powers of 2, being 2^i.
      eg. #(4 2 -1 -3) for X = 2^4 + 2^2 + 2^-1 + 2^-3."
  (let* (;; Copy of X
	 (X-cop (make-array (length X) :fill-pointer (length X)
				       :element-type 'fixnum))
	 (ncoins (length faces))
	 ;; Vector supporting the set of coins
	 (Ivec (make-array ncoins :fill-pointer ncoins
				  :element-type 'coin))
	 ;; Binary heap to always get the smallest coin a when popping.
	 (I nil)
	 ;; Solution vector of coins/packages to the problem.
	 (S (make-array 0 :fill-pointer 0 :element-type 'coin))
	 ;; Solution vector of initial coins as indexes in faces/weights.
	 ;; Returned if exists.
	 (sol-coins (make-array 0 :fill-pointer 0 :element-type 'fixnum)))
    ;; Make a copy of X.
    (loop for i from 0 below (length X) do
      (setf (aref X-cop i) (aref X i)))
    ;; Create the initial set of coins
    (loop for face across faces
	  for weight across weights
	  for id from 0 do
	    (setf (aref Ivec id) (make-coin :id id :face face :weight weight)))
    (setf I (binhp:make-heap Ivec #'coin-order))
    ;; Call the recursion, returns T on success, nil if there was no solution.
    (when (not (pm-rec-step I X-cop S)) (return-from pm-rec nil))
    ;; The solution is in S, in the shape of a forest of binary trees of coins.
    ;; We must gather all the leaves of all the trees.
    (loop for coinpack across S do
      (leafcoins coinpack sol-coins))
    sol-coins))

(defun pm-rec-step (I X S)
  "Recursion step for recursive package-merge."
  (when (and (= 0 (binhp:size I)) (> (length X) 0))
    ;; There is no solution to the problem.
    (return-from pm-rec-step nil))
  (when (= (length X) 0)
    ;; Recursion termination, we have paid all X with coins.
    (return-from pm-rec-step T))
  (let* ((a (binhp:peek I))
	 (r (coin-face a))
	 (minwidth (aref X (1- (length X)))))
    (when (> r minwidth)
      ;; No solution exists.
      (return-from pm-rec-step nil))
    (if (= r minwidth)
	;; Use a to pay r.
	(progn
	  ;; Add a to the solution and remove it from I
	  (vector-push-extend (binhp:extract I) S)
	  ;; Remove the part r of X that was paid.
	  (vector-pop X)
	  ;; Call pm-rec-step on the remaining problem
	  (pm-rec-step I X S))
	(progn
	  ;; If a is the sole remaining coin in I, throw it away.
	  (when (<= (binhp:size I) 1)
	    (binhp:extract I) 
	    (return-from pm-rec-step (pm-rec-step I X S)))
	  ;; Check the coin a' after a in I for its face value.
	  (let* ((ap (binhp:peek-second I))
		 (rp (coin-face ap)))
	    ;; If a' is not of same face value then throw away a and continue.
	    (when (/= r rp) (binhp:extract I)
		  (return-from pm-rec-step (pm-rec-step I X S)))
	    ;; Else make a package, insert it in I and continue.
	    (binhp:insert I (make-coin :face (1+ r)
				       :weight (+ (coin-weight a)
						  (coin-weight ap))
				       :left-coin (binhp:extract I)
				       :right-coin (binhp:extract I)))
	    (pm-rec-step I X S))))))
