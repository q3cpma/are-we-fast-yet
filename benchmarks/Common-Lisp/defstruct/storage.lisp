(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark")
  (load "som"))

(defpackage #:storage
  (:use #:cl #:benchmark)
  (:shadow #:count)
  (:export #:benchmark #:verify-result))
(in-package #:storage)


(defstruct (storage (:include benchmark)
					(:conc-name ""))
  (count 0))

(defmethod benchmark ((self storage))
  (setf (count self) 0)
  (build-tree-depth self 7 (som:make-random))
  (count self))

(defmethod verify-result ((self storage) result)
  (= result 5461))

(defmethod build-tree-depth ((self storage) depth random)
  (incf (count self))
  (if (= depth 1)
	  (make-array (+ (mod (som:next random) 10) 1))
	  (let ((arr (make-array 4)))
		(loop :for i :below 4
			  :do (setf (aref arr i) (build-tree-depth self (- depth 1) random)))
		arr)))

;; (format t "~A~%" (benchmark (make-instance 'storage))))
