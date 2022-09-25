(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark")
  (load "som"))

(defpackage #:storage
  (:use #:cl #:benchmark)
  (:shadow #:count)
  (:export #:benchmark #:verify-result))
(in-package #:storage)


(defclass storage (benchmark)
  ((count :accessor count :initform 0)))

(defmethod benchmark ((st storage))
  (setf (count st) 0)
  (build-tree-depth st 7 (som:make-random))
  (count st))

(defmethod verify-result ((st storage) result)
  (= result 5461))

(defmethod build-tree-depth ((st storage) depth random)
  (incf (count st))
  (if (= depth 1)
	  (make-array (+ (mod (som:next random) 10) 1))
	  (let ((arr (make-array 4)))
		(loop for i below 4
			  do (setf (aref arr i) (build-tree-depth st (- depth 1) random)))
		arr)))
