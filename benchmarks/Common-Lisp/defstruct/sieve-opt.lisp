(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark"))

(defpackage #:sieve-opt
  (:use #:cl #:benchmark)
  (:export #:benchmark #:verify-result))
(in-package #:sieve-opt)


(deftype positive-fixnum () `(integer 0 ,most-positive-fixnum))

(defstruct (sieve-opt (:include benchmark)))

(defmethod benchmark ((self sieve-opt))
  (let ((flags (make-array 5000 :element-type 'boolean :initial-element t)))
	(sieve self flags 5000)))

(defmethod verify-result ((self sieve-opt) result)
  (= result 669))

(defmethod sieve ((self sieve-opt) flags size)
  (declare (type (array boolean) flags)
		   (type positive-fixnum size))
  (loop :with prime-count :of-type positive-fixnum = 0
		:for i :of-type positive-fixnum :from 2 :upto size
		:if (aref flags (- i 1))
		  :do (incf prime-count)
		  :and :do (loop for k from (+ i i) :upto size :by i
						 :do (setf (aref flags (- k 1)) nil))
		:finally (return prime-count)))


;; (format t "~A~%" (benchmark (make-instance 'sieve-opt)))
