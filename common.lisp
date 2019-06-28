;;;; Common definitions for all the implemented algorithms.

(in-package :pm)

(defstruct coin
  ;; Face value of the coin. Stored as 'i' for the value '2^i'.
  (face 0 :type fixnum)
  ;; Numismatic value, or 'weight' of coin. Real number in the most general
  ;; case.
  (weigth 0))

