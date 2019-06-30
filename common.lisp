;;;; Common definitions for all the implemented algorithms.

(in-package :pm)

(defstruct coin
  ;; Unique id for coin, eg. index.
  (id -1 :type fixnum)
  ;; Face value of the coin. Stored as 'i' for the value '2^i'.
  (face 0 :type fixnum)
  ;; Numismatic value, or 'weight' of coin. Real number in the most general
  ;; case.
  (weight 0)
  ;; Each coin can be composed of two other coins, stored here. 
  (left-coin nil :type (or null coin))
  (right-coin nil :type (or null coin)))

(defun coin-order (coin1 coin2)
  "Order relation to sort coins in non decreasing order of face first and then
in non decreasing order of weight.
TODO: use nif here."
  (let ((f1 (coin-face coin1))
	(f2 (coin-face coin2)))
    (if (< f1 f2)
	T
	(if (= f1 f2)
	    (if (< (coin-weight coin1) (coin-weight coin2))
		T
		nil)
	    nil))))

