(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark")
  (load "som"))

(defpackage #:bounce
  (:use #:cl #:benchmark)
  (:shadow #:random)
  (:export #:benchmark #:verify-result))
(in-package #:bounce)


(defstruct (ball (:constructor make-ball-raw (x y x-vel y-vel))
				 (:conc-name ""))
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (x-vel 0 :type fixnum)
  (y-vel 0 :type fixnum))

(defun make-ball (rand)
  (make-ball-raw (mod (som:next rand) 500)
				 (mod (som:next rand) 500)
				 (- (mod (som:next rand) 300) 150)
				 (- (mod (som:next rand) 300) 150)))

(defmethod bounce ((self ball))
  (symbol-macrolet ((x (x self))
					(y (y self))
					(x-vel (x-vel self))
					(y-vel (y-vel self)))
	(let ((x-limit 500)
		  (y-limit 500)
		  (bounced nil))
	  (incf x x-vel)
	  (incf y y-vel)
	  (when (> x x-limit)
		(psetf x x-limit
			   x-vel (- (abs x-vel))
			   bounced t))
	  (when (< x 0)
		(psetf x 0
			   x-vel (abs x-vel)
			   bounced t))
	  (when (> y y-limit)
		(psetf y y-limit
			   y-vel (- (abs y-vel))
			   bounced t))
	  (when (< y 0)
		(psetf y 0
			   y-vel (abs y-vel)
			   bounced t))
	  bounced)))


(defstruct (bounce (:include benchmark)))

(defmethod benchmark ((self bounce))
  (let* ((rand (som:make-random))
		 (ball-count 100)
		 (balls (make-array ball-count :element-type 'ball)))
	(dotimes (i ball-count)
	  (setf (aref balls i) (make-ball rand)))
	(loop :repeat 50
		  :sum (loop :for ball :across balls
					 :count (bounce ball)))))

(defmethod verify-result ((self bounce) result)
  (= result 1331))


;; (format t "~A~%" (benchmark (make-bounce)))
