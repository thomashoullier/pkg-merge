;;;; Performance tests, mainly for non-regression.

(let* ((ncoins 100000)
       (nX 60)
       (maxweight 100)
       (maxface 50)			;+-
       (faces ( make-array ncoins :element-type 'fixnum))
       (weights (make-array ncoins :element-type 'fixnum))
       (X (make-array nX :element-type 'fixnum))
       (Xval 0)
       (recur)
       (nonrecur))
  ;; Filling all with random data.
  (loop for i from 0 below ncoins do
    (setf (aref weights i) (random maxweight))
    (setf (aref faces i) (- (random (* 2 maxface)) maxface)))
  (loop for i from 0 below nX do
    (setf (aref X i) (- (random (* 2 maxface)) maxface)))
  (setf X (delete-duplicates (sort X #'>)))
  ;; Computing the face value of X.
  (loop for f across X do
    (incf Xval (expt 2 f)))
  ;; Computing the two results.
  (format t "Recursive:~%")
  (time (setf recur (pm:pm-rec faces weights X)))
  (format t "Non-recursive:~%")
  (time (setf nonrecur (pm:pm-nonrec faces weights X)))
  ;; Also check while we're at it.
  (check-inds recur nonrecur faces weights Xval))
