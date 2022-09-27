(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark"))

(defpackage #:list
  (:use #:cl #:benchmark)
  (:shadow #:length #:make-list #:copy-list #:list)
  (:export #:benchmark #:verify-result))
(in-package #:list)


(defstruct (element (:conc-name ""))
  val
  (next nil))

(defmethod length ((self element))
  (if (next self)
	  (+ 1 (length (next self)))
	  1))


(defstruct (list (:include benchmark)
				 (:constructor make-list-raw)))

(defmethod benchmark ((self list))
  (length
   (tail self
		 (make-list 15)
		 (make-list 10)
		 (make-list 6))))

(defmethod verify-result ((self list) result)
  (= result 10))

(defun make-list (length)
  (if (= length 0)
	  nil
	  (let ((e (make-element :val length)))
		(setf (next e) (make-list (- length 1)))
		e)))

(defun is-shorter-than (x y)
  (let ((x-tail x)
		(y-tail y))
	(loop while y-tail
		  if (null x-tail)
			return t
		  do (setf x-tail (next x-tail)
				   y-tail (next y-tail))
		  finally (return nil))))

(defmethod tail ((self list) x y z)
  (if (is-shorter-than y x)
	  (tail self
			(tail self (next x) y z)
			(tail self (next y) z x)
			(tail self (next z) x y))
	  z))


;; (format t "~A~%" (benchmark (make-instance 'list)))
