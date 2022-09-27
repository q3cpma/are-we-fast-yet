(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark"))

(defpackage #:towers
  (:use #:cl #:benchmark)
  (:export #:benchmark #:verify-result))
(in-package #:towers)


(defclass towers-disk ()
  ((size :accessor size :initarg :size)
   (next :accessor next :initform nil)))


(defclass towers (benchmark)
  ((piles :accessor piles :initform nil)
   (moves-done :accessor moves-done :initform 0)))

(defmethod benchmark ((self towers))
  (setf (piles self) (make-array 3 :initial-element nil)
		(moves-done self) 0)
  (build-tower-at self 0 13)
  (move-disks self 13 0 1)
  (moves-done self))

(defmethod verify-result ((self towers) result)
  (= result 8191))

(defmethod push-disk ((self towers) disk pile)
  (let ((top (aref (piles self) pile)))
	(if (and top (>= (size disk) (size top)))
		(error "Cannot put a big disk on a smaller one"))
	(setf (next disk) top
		  (aref (piles self) pile) disk)))

(defmethod pop-disk-from ((self towers) pile)
  (let ((top (aref (piles self) pile)))
	(if (null top)
		(error "Attempting to remove a disk from an empty pile"))
	(setf (aref (piles self) pile) (next top)
		  (next top) nil)
	top))

(defmethod move-top-disk ((self towers) from-pile to-pile)
  (push-disk self (pop-disk-from self from-pile) to-pile)
  (incf (moves-done self)))

(defmethod build-tower-at ((self towers) pile disks)
  (loop for i from disks downto 0
		do (push-disk self (make-instance 'towers-disk :size i) pile)))

(defmethod move-disks ((self towers) disks from-pile to-pile)
  (if (= disks 1)
	  (move-top-disk self from-pile to-pile)
	  (let ((other-pile (- 3 from-pile to-pile)))
		(move-disks self (- disks 1) from-pile other-pile)
		(move-top-disk self from-pile to-pile)
		(move-disks self (- disks 1) other-pile to-pile))))


;; (format t "~A~%" (benchmark (make-instance 'towers)))
