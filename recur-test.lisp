;;;; Recursive implementation pm-rec testing.

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
       (S (pm:pm-rec faces weights X))
       (Xval 0))
  (loop for f across X do
    (incf Xval (expt 2 f)))
  (check-inds possible-S S faces weights Xval))

;; Test 3:
(let* ((faces #(-1 -1 -1 -1 -1 -1 -1 0 0))
       (weights #(2 2 2 2 2 2 2 6 1))
       (X #(1))
       (possible-S #(0 1 8))
       (Xval 0))
  (loop for f across X do
    (incf Xval (expt 2 f)))
  (check-inds possible-S (pm:pm-rec faces weights X) faces weights Xval))

;; Test 4: README example
(let* ((faces #(-2 -2 -2 -2 -1 -1 1 1))
       (weights #(1 1 3 4 2 5 1 3))
       (X #(1 0 -2))
       (possible-S #(0 1 2 4 6))
       (Xval 0))
  (loop for f across X do
    (incf Xval (expt 2 f)))
  (check-inds possible-S (pm:pm-rec faces weights X) faces weights Xval))

(format t "recur-test: PASSED.~%")
