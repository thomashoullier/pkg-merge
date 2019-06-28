;;;; Common definitions for all the implemented algorithms.

(in-package :pm)

(defstruct coin
  ;; Unique identifier for the coin, can be an index etc.
  ;; Useful to return the solution set of coins.
  (id 0)
  ;; Face value of the coin. Stored as 'i' for the value '2^i'.
  (face 0 :type fixnum)
  ;; Numismatic value, or 'weight' of coin. Real number in the most general
  ;; case.
  (weight 0)
  ;; Tail, to keep track of which coin is in which package.
  (tail nil :type (or null coin)))

(defun coin-order (coin1 coin2)
  "Order relation to sort coins in non decreasing order of face first and then
in non decreasing order of weight.
TODO: use nif here."
  (let ((f1 (coin-face coin1))
	(f2 (coin-face coin2)))
    (if (< f1 f2)
	T
	(if (= f1 f2)
	    (if (<= (coin-weight coin1) (coin-weight coin2))
		T
		nil)
	    nil))))

