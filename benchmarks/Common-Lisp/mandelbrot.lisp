(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark")
  (setf *read-default-float-format* 'double-float))

(defpackage #:mandelbrot
  (:use #:cl #:benchmark)
  (:export #:benchmark #:verify-result))
(in-package #:mandelbrot)


(deftype positive-fixnum ()
  `(integer 0 ,most-positive-fixnum))

(defun mandelbrot (size)
  (declare (type positive-fixnum size))
  (loop with sum = 0
		and byte-acc = 0
		and bit-num = 0
		for y below size
		for ci of-type double-float = (- (/ (* 2.0 y) size) 1.0)
		do (loop for x below size
				 for zr   of-type double-float = 0.0
				 for zrzr of-type double-float = 0.0
				 for zi   of-type double-float = 0.0
				 for zizi of-type double-float = 0.0
				 for cr   of-type double-float = (- (/ (* 2.0 x) size) 1.5)
				 for z    of-type positive-fixnum = 0
				 for not-done = t
				 for escape = 0
				 do (loop while (and not-done (< z 50))
						  do (setf zr (+ (- zrzr zizi) cr)
								   zi (+ (* 2.0 zr zi) ci)
								   zrzr (* zr zr)
								   zizi (* zi zi))
						  if (> (+ zrzr zizi) 4.0)
							do (setf not-done nil
									 escape 1)
						  do (incf z))
				 do (setf byte-acc (+ (ash byte-acc 1) escape)
						  bit-num (+ bit-num 1))
				 if (= bit-num 8)
				   do (setf sum (logxor sum byte-acc)
							byte-acc 0
							bit-num 0)
				 else if (= x (- size 1))
					do (setf byte-acc (ash byte-acc (- 8 bit-num))
							 sum (logxor sum byte-acc)
							 byte-acc 0
							 bit-num 0))
		finally (return sum)))

(defstruct (mandelbrot (:include benchmark)
                       (:conc-name "")))

(defmethod inner-benchmark-loop ((self mandelbrot) inner-iterations)
  (verify-result-override (mandelbrot inner-iterations) inner-iterations))

(defun verify-result-override (result inner-iterations)
  (cond
	((= inner-iterations 500) (= result 191))
	((= inner-iterations 750) (= result 50))
	((= inner-iterations 1)   (= result 128))
	(t (progn
		 (format t "No verification result for ~D found~%" inner-iterations)
		 (format t "Result is: ~D~%" result)
		 nil))))

(defmethod benchmark ((self mandelbrot))
  (error "Should never be reached"))

(defmethod verify-result ((self mandelbrot) result)
  (declare (ignore result))
  (error "Should never be reached"))


;; (format t "~A~%" (mandelbrot 750))
