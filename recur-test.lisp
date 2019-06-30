;;;; Recursive implementation pm-rec testing.

;;; Manual test cases.
;; Test 1: No solution.
(let ((faces #(-1 -1 -1 0 0 1))
      (weights #(10 2 2 4 5 10))
      (X #(-1 0 1 2)))
  (assert (not (pm:pm-rec faces weights X))))


