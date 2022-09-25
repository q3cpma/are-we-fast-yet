(defpackage #:som
  (:use #:cl)
  (:shadow #:random)
  (:export
   #:random
   #:make-random
   #:next))
(in-package #:som)


(defstruct random
  (seed 0 :type fixnum))

(defmethod next ((r random))
  (setf (random-seed r)
		(logand (+ (* (random-seed r) 1309) 13849) 65535)))
