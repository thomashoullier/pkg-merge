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
       (S (pm:pm-rec faces weights X)))
  (check-inds possible-S S faces weights))

;; Test 3:
(let* ((faces #(-1 -1 -1 -1 -1 -1 -1 0 0))
       (weights #(2 2 2 2 2 2 2 6 1))
       (X #(1))
       (possible-S #(0 1 8)))
  (check-inds possible-S (pm:pm-rec faces weights X) faces weights))

(format t "recur-test: PASSED.~%")
