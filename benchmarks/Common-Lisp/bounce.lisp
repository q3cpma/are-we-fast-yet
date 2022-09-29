(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark")
  (load "som"))

(defpackage #:bounce
  (:use #:cl #:benchmark)
  (:shadow #:random)
  (:export #:benchmark #:verify-result))
(in-package #:bounce)


(defclass ball ()
  ((x :accessor x)
   (y :accessor y)
   (x-vel :accessor x-vel)
   (y-vel :accessor y-vel)))

(defmethod initialize-instance :after ((self ball) &rest args &key rand)
  (declare (ignore args))
  (assert rand)
  (setf (x self) (mod (som:next rand) 500)
		(y self) (mod (som:next rand) 500)
		(x-vel self) (- (mod (som:next rand) 300) 150)
		(y-vel self) (- (mod (som:next rand) 300) 150)))

(defmethod bounce ((self ball))
  (with-slots (x y x-vel y-vel) self
	(let ((x-limit 500)
		  (y-limit 500)
		  (bounced nil))
	  (incf x x-vel)
	  (incf y y-vel)
	  (when (> x x-limit)
		(setf x x-limit
			  x-vel (- (abs x-vel))
			  bounced t))
	  (when (< x 0)
		(setf x 0
			  x-vel (abs x-vel)
			  bounced t))
	  (when (> y y-limit)
		(setf y y-limit
			  y-vel (- (abs y-vel))
			  bounced t))
	  (when (< y 0)
		(setf y 0
			  y-vel (abs y-vel)
			  bounced t))
	  bounced)))


(defclass bounce (benchmark) ())

(defmethod benchmark ((self bounce))
  (let* ((rand (som:make-random))
		 (ball-count 100)
		 (balls (make-array ball-count)))
	(dotimes (i ball-count)
	  (setf (aref balls i) (make-instance 'ball :rand rand)))
	(loop repeat 50
		  sum (loop for ball across balls
					count (bounce ball)))))

(defmethod verify-result ((self bounce) result)
  (= result 1331))


;; (format t "~A~%" (benchmark (make-instance 'bounce)))
