;;;; Recursive algorithm for package-merge.
;;;; This is the general case of package-merge, not specialized for
;;;; length-limited Huffman coding.

(in-package :pm)

(defun pm-rec (I-init X)
  " Recursive version of package-merge.
I: I-init: Vector of coins. In any order.
   X: Vector of i values in increasing order. The number X represents is a 
      decomposition in powers of 2, being 2^i.
      eg. #(-3 -1 2 4) for X = 2^-3 + 2^-1 + 2^2 + 2^4."
  (let (;; Binary heap to always get the smallest coin a when popping.
	(I (binhp:make-heap I-init #'coin-order))
	;; Solution vector of coins/packages to the problem.
	(S (make-array 0 :fill-pointer 0)))
    ;; Call the recursion, returns T on success, nil if there was no solution.
    (when (not (pm-rec-step I X S)) (return-from pm-rec nil))
    ;; The solution is in S, in the shape of a forest of binary trees of coins.
    ;; We must gather all the leaves of all the trees.

    ))

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
	 (minwidth (aref X 0)))
    (when (> r minwidth)
      ;; No solution exists.
      (return-from pm-rec-step nil))
    (if (= r minwidth)
	;; Use a to pay r.
	(progn
	  ;; Add a to the solution and remove it from I
	  (vector-push-extend (vector-pop I) S)
	  ;; Remove the part r of X that was paid.
	  (vector-pop X)
	  ;; Call pm-rec-step on the remaining problem
	  (pm-rec-step I X S))
	(progn
	  ;; If a is the sole remaining coin in I, throw it away.
	  (when (<= (binhp:size I) 1) (vector-pop I) (pm-rec-step I X S))
	  ;; Check the coin a' after a in I for its face value.
	  (let* ((ap (binhp:peek-second I))
		 (rp (coin-face ap)))
	    ;; If a' is not of same face value then throw away a and continue.
	    (when (/= r rp) (vector-pop I) (pm-rec-step I X S))
	    ;; Else make a package, insert it in I and continue.
	    (binhp:insert I (make-coin :face (+ r rp)
				       :weight (+ (coin-weight a)
						  (coin-weight ap))
				       :left-coin (vector-pop I)
				       :right-coin (vector-pop I)))
	    (pm-rec-step I X S))))))
