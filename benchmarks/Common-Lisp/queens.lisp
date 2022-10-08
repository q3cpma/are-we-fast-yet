(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark"))

(defpackage #:queens
  (:use #:cl #:benchmark)
  (:export #:benchmark #:verify-result))
(in-package #:queens)


(deftype positive-fixnum () `(integer 0 ,most-positive-fixnum))

(defstruct (queens (:include benchmark)
				   (:conc-name ""))
  (free-rows #() :type (array boolean))
  (free-maxs #() :type (array boolean))
  (free-mins #() :type (array boolean))
  (queen-rows (make-array 0 :element-type 'fixnum) :type (array fixnum)))

(defmethod benchmark ((self queens))
  (loop :repeat 10 :always (queens self)))

(defmethod verify-result ((self queens) result)
  result)

(defmethod queens ((self queens))
  (setf (free-rows self) (make-array 8 :initial-element t)
		(free-maxs self) (make-array 16 :initial-element t)
		(free-mins self) (make-array 16 :initial-element t)
		(queen-rows self) (make-array 8 :element-type 'fixnum :initial-element -1))
  (place-queen self 0))

(defmethod place-queen ((self queens) c)
  (declare (type positive-fixnum c))
  (dotimes (r 8 nil)
	(declare (type positive-fixnum r))
	(when (get-row-column self r c)
	  (setf (aref (queen-rows self) r) c)
	  (set-row-column self r c nil)

	  (if (= c 7)
		  (return t))
	  (if (place-queen self (+ c 1))
		  (return t))

	  (set-row-column self r c t))))

(defmethod get-row-column ((self queens) r c)
  (declare (type positive-fixnum r c))
  (and
   (free-rows self)
   (aref (free-maxs self) (+ c r))
   (aref (free-mins self) (+ (- c r) 7))))

(defmethod set-row-column ((self queens) r c v)
  (declare (type positive-fixnum r c)
		   (type boolean v))
  (setf (aref (free-rows self) r) v
		(aref (free-maxs self) (+ c r)) v
		(aref (free-mins self) (+ (- c r) 7)) v))


;; (format t "~A~%" (benchmark (make-queens)))
