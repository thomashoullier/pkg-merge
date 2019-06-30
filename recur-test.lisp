;;;; Recursive implementation pm-rec testing.

;;; Helpers
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

(defun check-inds (ind1 ind2 faces weights)
"Check two sets of indexes in faces/weights to see if they point to coins
of the same value.
ind1 is the reference, ind2 is the sequence to validate."
  (assert (= (length ind1) (length ind2)))
  (let ((ic1 (make-array (length ind1) :fill-pointer (length ind1)))
	(ic2 (make-array (length ind2) :fill-pointer (length ind2)))
	(c1 (make-array 0 :fill-pointer 0 :element-type 'coin-test))
	(c2 (make-array 0 :fill-pointer 0 :element-type 'coin-test))
	(ibuf 0))
    ;; Fill both vectors
    (loop for i from 0 below (length ind1) do
      (psetf (aref ic1 i) (aref ind1 i)
	     (aref ic2 i) (aref ind2 i)))
    ;; Sort both vectors
    (psetf ic1 (sort ic1 #'<) ic2 (sort ic2 #'<))
    ;; Indexes that are the same in both lists are obviously the same coins.
    (loop for i1 across ic1
	  for i2 across ic2
	  for i from 0 do
	    (when (= i1 i2)
	      (psetf (aref ic1 (1- (length ic1))) (aref ic1 i)
		     (aref ic2 (1- (length ic2))) (aref ic2 i))
	      (vector-pop ic1) (vector-pop ic2)))
    ;; Sort again
    (psetf ic1 (sort ic1 #'<) ic2 (sort ic2 #'<))
    ;; Check there are no duplicates in ic2
    (setf ibuf (aref ic2 0))
    (loop for i from 1 below (length ic2) do
      (assert (/= (aref ic2 i) ibuf))
      (setf ibuf (aref ic2 i)))
    ;; Now compare the indexes that are not the same.
    ;; Pull all the coins, sort them and compare.
    (loop for i1 across ic1
	  for i2 across ic2 do
	    (vector-push-extend (make-coin-test :face (aref faces i1)
						:weight (aref weights i1))
				c1)
	    (vector-push-extend (make-coin-test :face (aref faces i2)
						:weight (aref weights i2))
				c2))
    (psetf c1 (sort c1 #'coin-order)
	   c2 (sort c2 #'coin-order))
    ;; Compare the coins one by one.
    (loop for cc1 across c1
	  for cc2 across c2 do
	    (assert (and (= (coin-test-face cc1) (coin-test-face cc2))
			 (= (coin-test-weight cc1) (coin-test-weight cc2)))))))

;;; Manual test cases.
;; Test 1: No solution.
(let ((faces #(-1 -1 -1 0 0 1))
      (weights #(10 2 2 4 5 10))
      (X #(2 1 0 -1)))
  (assert (not (pm:pm-rec faces weights X))))

;; Test 2:
(let* ((faces #(-1 0 1 2 2 -1 -1 0))
       (weights #(10 4 10 15 3 2 2 5))
       (X #(2 1 0 -1))
       (possible-S #(1 2 4 5))
       (S (pm:pm-rec faces weights X)))
  (check-inds possible-S S faces weights))

;; Test 3:
(let* ((faces #(-1 -1 -1 -1 -1 -1 -1 0 0))
       (weights #(2 2 2 2 2 2 2 6 1))
       (X #(1))
       (possible-S #(0 1 8)))
  (check-inds possible-S (pm:pm-rec faces weights X) faces weights))

(format t "recur-test: PASSED.~%")
