(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark"))

(defpackage #:towers-opt
  (:use #:cl #:benchmark)
  (:export #:benchmark #:verify-result))
(in-package #:towers-opt)


(deftype positive-fixnum () `(integer 0 ,most-positive-fixnum))

(defstruct (towers-disk (:conc-name ""))
  (size 0 :type positive-fixnum)
  (next nil :type (or null towers-disk)))


(defstruct (towers-opt (:include benchmark)
					   (:conc-name ""))
  (piles #() :type (array))
  (moves-done 0 :type positive-fixnum))

(defmethod benchmark ((self towers-opt))
  (setf (piles self) (make-array 3 :initial-element nil)
		(moves-done self) 0)
  (build-tower-at self 0 13)
  (move-disks self 13 0 1)
  (moves-done self))

(defmethod verify-result ((self towers-opt) result)
  (= result 8191))

(defmethod push-disk ((self towers-opt) disk pile)
  (declare (type positive-fixnum pile)
		   (type towers-disk disk))
  (let ((top (aref (piles self) pile)))
	(if (and top (>= (size disk) (size top)))
		(error "Cannot put a big disk on a smaller one"))
	(setf (next disk) top
		  (aref (piles self) pile) disk)))

(defmethod pop-disk-from ((self towers-opt) pile)
  (declare (type positive-fixnum pile))
  (let ((top (aref (piles self) pile)))
	(if (null top)
		(error "Attempting to remove a disk from an empty pile"))
	(setf (aref (piles self) pile) (next top)
		  (next top) nil)
	top))

(defmethod move-top-disk ((self towers-opt) from-pile to-pile)
  (declare (type positive-fixnum from-pile to-pile))
  (push-disk self (pop-disk-from self from-pile) to-pile)
  (incf (moves-done self)))

(defmethod build-tower-at ((self towers-opt) pile disks)
  (loop :for i :from disks :downto 0
		:do (push-disk self (make-towers-disk :size i) pile)))

(defmethod move-disks ((self towers-opt) disks from-pile to-pile)
  (declare (type positive-fixnum disks from-pile to-pile))
  (if (= disks 1)
	  (move-top-disk self from-pile to-pile)
	  (let ((other-pile (- 3 from-pile to-pile)))
		(move-disks self (- disks 1) from-pile other-pile)
		(move-top-disk self from-pile to-pile)
		(move-disks self (- disks 1) other-pile to-pile))))


;; (format t "~A~%" (benchmark (make-instance 'towers-opt)))
