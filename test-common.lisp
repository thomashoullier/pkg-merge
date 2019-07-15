;;;; Helpers common to all the tests.

(defstruct coin-test
  (face)
  (weight))

(defun coin-order (coin1 coin2)
  "Order relation to sort coins in non decreasing order of face first and then
in non decreasing order of weight.
TODO: use nif here."
  (let ((f1 (coin-test-face coin1))
	(f2 (coin-test-face coin2)))
    (if (< f1 f2)
	T
	(if (= f1 f2)
	    (if (< (coin-test-weight coin1) (coin-test-weight coin2))
		T
		nil)
	    nil))))

(defun check-inds (ind1 ind2 faces weights Xval)
  "Checks that both ind1 and ind2 respect:
* No duplicates.
* Pay for X.
* Amount to the same weight value.
I : ind1, ind2: The two sets of coins to check.
    faces, weights: 
    Xval: The sum of 2^d terms in X."
  ;; If both are nil, return OK
  (when (or (not ind1) (not ind2))
    (if (and (not ind1) (not ind2))
	(return-from check-inds T)
	(error "One set is empty and not the other.")))
  (let ((ind-buf 0)
	(w1 0)
	(f1 0)
	(w2 0)
	(f2 0))
    ;; Check that neither set has duplicates.
    (loop for ind in (list ind1 ind2) do
      (loop for i from 0 below (length ind) do
	(setf ind-buf (aref ind i))
	(loop for j from (1+ i) below (length ind) do
	      (assert (/= ind-buf (aref ind j))))))
    ;; Accumulate the faces and weights.
    (loop for i across ind1 do
      (incf w1 (aref weights i))
      (incf f1 (expt 2 (aref faces i))))
    (loop for i across ind2 do
      (incf w2 (aref weights i))
      (incf f2 (expt 2 (aref faces i))))
    ;; Check against X and weights against each other.
    (assert (= Xval f1 f2))
    (assert (= w1 w2))))
