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
	;; Vector of the IDs of the solution coins to the problem.
	(S-id (make-array 0 :fill-pointer 0)))
    ;; Call the recursion, returns T on success, nil if there was no solution.
    ))

(defun pm-rec-step (I X S-id)
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
    
    ))
