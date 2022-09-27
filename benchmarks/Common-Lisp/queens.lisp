(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark"))

(defpackage #:queens
  (:use #:cl #:benchmark)
  (:export #:benchmark #:verify-result))
(in-package #:queens)


(defclass queens (benchmark)
  ((free-rows :accessor free-rows)
   (free-maxs :accessor free-maxs)
   (free-mins :accessor free-mins)
   (queen-rows :accessor queen-rows)))

(defmethod benchmark ((self queens))
  (loop repeat 10 always (queens self)))

(defmethod verify-result ((self queens) result)
  result)

(defmethod queens ((self queens))
  (setf (free-rows self) (make-array 8 :initial-element t)
		(free-maxs self) (make-array 16 :initial-element t)
		(free-mins self) (make-array 16 :initial-element t)
		(queen-rows self) (make-array 8 :initial-element -1))
  (place-queen self 0))

(defmethod place-queen ((self queens) c)
  (dotimes (r 8 nil)
	(when (get-row-column self r c)
	  (setf (aref (queen-rows self) r) c)
	  (set-row-column self r c nil)

	  (if (= c 7)
		  (return t))
	  (if (place-queen self (+ c 1))
		  (return t))

	  (set-row-column self r c t))))

(defmethod get-row-column ((self queens) r c)
  (and
   (free-rows self)
   (aref (free-maxs self) (+ c r))
   (aref (free-mins self) (+ (- c r) 7))))

(defmethod set-row-column ((self queens) r c v)
  (setf (aref (free-rows self) r) v
		(aref (free-maxs self) (+ c r)) v
		(aref (free-mins self) (+ (- c r) 7)) v))


;; (format t "~A~%" (benchmark (make-instance 'queens)))
