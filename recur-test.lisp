;;;; Recursive implementation pm-rec testing.

;;; Helpers

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
  (format t "~A~%" S))
