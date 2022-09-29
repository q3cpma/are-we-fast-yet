(defpackage #:som
  (:use #:cl)
  (:shadow #:random)
  (:export
   #:random
   #:make-random
   #:next))
(in-package #:som)


(deftype positive-fixnum ()
  `(integer 0 ,most-positive-fixnum))

(defstruct random
  (seed 74755 :type positive-fixnum))

(defmethod next ((self random))
  (setf (random-seed self) (logand (+ (* (random-seed self) 1309) 13849) 65535)))

(declaim (ftype (function (random) positive-fixnum) next)
		 (inline next))
