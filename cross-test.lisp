;;;; Cross-checking the two implementations. Uses random tests.

(let* ((ncoins (1+ (random 20)))
       (nX (1+ (random 10)))
       (maxweight (1+ (random 40)))
       (maxface (1+ (random 5))) ;+-
       (faces (make-array ncoins :element-type 'fixnum))
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
  (psetf recur (pm:pm-rec faces weights X)
	 nonrecur (pm:pm-nonrec faces weights X))
  (format t "~A~%~A~%~A~%~A~%~A" faces weights X recur nonrecur)
  (check-inds recur nonrecur faces weights Xval))
