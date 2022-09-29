(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark")
  (load "som"))

(defpackage #:storage-opt
  (:use #:cl #:benchmark)
  (:shadow #:count)
  (:export #:benchmark #:verify-result))
(in-package #:storage-opt)


(deftype positive-fixnum () `(integer 0 ,most-positive-fixnum))

(defstruct (storage-opt (:include benchmark)
						(:conc-name ""))
  (count 0 :type positive-fixnum))

(defmethod benchmark ((self storage-opt))
  (setf (count self) 0)
  (build-tree-depth self 7 (som:make-random))
  (count self))

(defmethod verify-result ((self storage-opt) result)
  (= result 5461))

(defmethod build-tree-depth ((self storage-opt) depth random)
  (declare (type positive-fixnum depth))
  (incf (count self))
  (if (= depth 1)
	  (make-array (+ (mod (som:next random) 10) 1))
	  (let ((arr (make-array 4)))
		(loop :for i :below 4
			  :do (setf (aref arr i) (build-tree-depth self (- depth 1) random)))
		arr)))

;; (format t "~A~%" (benchmark (make-instance 'storage-opt))))
