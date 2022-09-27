(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark"))

(defpackage #:sieve
  (:use #:cl #:benchmark)
  (:export #:benchmark #:verify-result))
(in-package #:sieve)


(defstruct (sieve (:include benchmark)))

(defmethod benchmark ((self sieve))
  (let ((flags (make-array 5000 :element-type 'boolean :initial-element t)))
	(sieve self flags 5000)))

(defmethod verify-result ((self sieve) result)
  (= result 669))

(defmethod sieve ((self sieve) flags size)
  (loop with prime-count = 0
		for i from 2 upto size
		if (aref flags (- i 1))
		  do (incf prime-count)
		  and do (loop for k from (+ i i) upto size by i
					   do (setf (aref flags (- k 1)) nil))
		end
		finally (return prime-count)))


;; (format t "~A~%" (benchmark (make-instance 'sieve)))
