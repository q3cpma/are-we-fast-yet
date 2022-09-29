(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark"))

(defpackage #:permute
  (:use #:cl #:benchmark)
  (:shadow #:count)
  (:export #:benchmark #:verify-result))
(in-package #:permute)


(defstruct (permute (:include benchmark)
					(:conc-name ""))
  (count 0)
  v)

(defmethod benchmark ((self permute))
  (setf (count self) 0
		(v self) (make-array 6 :initial-element 0))
  (permute self 6)
  (count self))

(defmethod verify-result ((self permute) result)
  (= result 8660))

(defmethod permute ((self permute) n)
  (incf (count self))
  (when (/= n 0)
	(let ((n1 (- n 1)))
	  (permute self n1)
	  (loop :for i :from n1 :downto 0
			:do (swap self n1 i)
			:do (permute self n1)
			:do (swap self n1 i)))))

(defmethod swap ((self permute) i j)
  (with-slots (v) self
	(let ((tmp (aref v i)))
	  (setf (aref v i) (aref v j)
			(aref v j) tmp))))


;; (format t "~A~%" (benchmark (make-instance 'permute)))
