(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "benchmark")
  (setf *read-default-float-format* 'double-float))

(defpackage #:nbody-opt
  (:use #:cl #:benchmark)
  (:export #:benchmark #:verify-result))
(in-package #:nbody-opt)

(deftype positive-fixnum () `(integer 0 ,most-positive-fixnum))

(defconstant +solar-mass+ (coerce (* 4 pi pi) 'double-float))
(defconstant +days-per-year+ 365.24)

(defstruct (body (:constructor make-body-raw (x y z vx vy vz mass))
				 (:conc-name ""))
  (x    0.0 :type double-float)
  (y    0.0 :type double-float)
  (z    0.0 :type double-float)
  (vx   0.0 :type double-float)
  (vy   0.0 :type double-float)
  (vz   0.0 :type double-float)
  (mass 0.0 :type double-float))

(defun make-body (&key x y z vx vy vz mass)
  (make-body-raw x y z
				 (* vx +days-per-year+)
				 (* vy +days-per-year+)
				 (* vz +days-per-year+)
				 (* mass +solar-mass+)))

(defmethod offset-momentum ((self body) px py pz)
  (psetf (vx self) (- (/ px +solar-mass+))
		 (vy self) (- (/ py +solar-mass+))
		 (vz self) (- (/ pz +solar-mass+))))

(defun jupiter ()
  (make-body
				 :x    4.84143144246472090e00
				 :y   -1.16032004402742839e00
				 :z   -1.03622044471123109e-01
				 :vx   1.66007664274403694e-03
				 :vy   7.69901118419740425e-03
				 :vz  -6.90460016972063023e-05
				 :mass 9.54791938424326609e-04))

(defun saturn ()
  (make-body
				 :x    8.34336671824457987e00
				 :y    4.12479856412430479e00
				 :z   -4.03523417114321381e-01
				 :vx  -2.76742510726862411e-03
				 :vy   4.99852801234917238e-03
				 :vz   2.30417297573763929e-05
				 :mass 2.85885980666130812e-04))

(defun uranus ()
  (make-body
				 :x    1.28943695621391310e01
				 :y   -1.51111514016986312e01
				 :z   -2.23307578892655734e-01
				 :vx   2.96460137564761618e-03
				 :vy   2.37847173959480950e-03
				 :vz  -2.96589568540237556e-05
				 :mass 4.36624404335156298e-05))

(defun neptune ()
  (make-body
				 :x    1.53796971148509165e01
				 :y   -2.59193146099879641e01
				 :z    1.79258772950371181e-01
				 :vx   2.68067772490389322e-03
				 :vy   1.62824170038242295e-03
				 :vz  -9.51592254519715870e-05
				 :mass 5.15138902046611451e-05))

(defun sun ()
  (make-body :x 0.0 :y 0.0 :z 0.0 :vx 0.0 :vy 0.0 :vz 0.0 :mass 1.0))


(defstruct (nbody-system (:conc-name ""))
  (bodies (create-bodies)))

(defun create-bodies ()
  (let ((bodies (make-array 5 :initial-contents (list (sun) (jupiter) (saturn) (uranus) (neptune)))))
	(loop :with px = 0.0 :and py = 0.0 :and pz = 0.0
		  :for b :across bodies
		  :do (incf px (* (vx b) (mass b)))
		  :do (incf py (* (vy b) (mass b)))
		  :do (incf pz (* (vz b) (mass b)))
		  :finally (offset-momentum (aref bodies 0) px py pz))
	bodies))

(defmethod advance ((self nbody-system) dt)
  (declare (type double-float dt))
  (loop :for i :below (length (bodies self))
		:for i-body :of-type body = (aref (bodies self) i)
		:do (loop :for j :from (+ i 1) :below (length (bodies self))
				  :for j-body    :of-type body = (aref (bodies self) j)
				  :for dx        :of-type double-float = (- (x i-body) (x j-body))
				  :for dy        :of-type double-float = (- (y i-body) (y j-body))
				  :for dz        :of-type double-float = (- (z i-body) (z j-body))
				  :for d-squared :of-type double-float = (+ (* dx dx) (* dy dy) (* dz dz))
				  :for distance  :of-type double-float = (sqrt d-squared)
				  :for mag       :of-type double-float = (/ dt (* d-squared distance))
				  :do (psetf (vx i-body) (- (vx i-body) (* dx (mass j-body) mag))
							 (vy i-body) (- (vy i-body) (* dy (mass j-body) mag))
							 (vz i-body) (- (vz i-body) (* dz (mass j-body) mag))
							 (vx j-body) (+ (vx j-body) (* dx (mass i-body) mag))
							 (vy j-body) (+ (vy j-body) (* dy (mass i-body) mag))
							 (vz j-body) (+ (vz j-body) (* dz (mass i-body) mag)))))
  (loop :for body :across (bodies self)
		:do (psetf (x body) (+ (x body) (* dt (vx body)))
				   (y body) (+ (y body) (* dt (vy body)))
				   (z body) (+ (z body) (* dt (vz body))))))

(defmethod energy ((self nbody-system))
  (loop :with e = 0.0
		:for i :below (length (bodies self))
		:for i-body = (aref (bodies self) i)
		:do (incf e (* 0.5
					   (mass i-body)
					   (+ (* (vx i-body) (vx i-body))
						  (* (vy i-body) (vy i-body))
						  (* (vz i-body) (vz i-body)))))
		:do (loop :for j :from (+ i 1) :below (length (bodies self))
				  :for j-body = (aref (bodies self) j)
				  :for dx = (- (x i-body) (x j-body))
				  :for dy = (- (y i-body) (y j-body))
				  :for dz = (- (z i-body) (z j-body))
				  :for distance = (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
				  :do (decf e (/
							   (* (mass i-body) (mass j-body))
							   distance)))
		:finally (return e)))


(defstruct (nbody-opt (:include benchmark)))

(defmethod inner-benchmark-loop ((self nbody-opt) inner-iterations)
  (loop with system = (make-nbody-system)
		repeat inner-iterations
		do (advance system 0.01)
		finally (return (verify-result-override (energy system) inner-iterations))))

(defun verify-result-override (result inner-iterations)
  (cond
	((= inner-iterations 250000) (= result -0.1690859889909308))
	((= inner-iterations 1)      (= result -0.16907495402506745))
	(t (progn
		 (format t "No verification result for ~D found~%" inner-iterations)
		 (format t "Result is: ~D~%" result)
		 nil))))

(defmethod benchmark ((self nbody-opt))
  (error "Should never be reached"))

(defmethod verify-result ((self nbody-opt) result)
  (declare (ignore result))
  (error "Should never be reached"))


;; (format t "~A~%" (inner-benchmark-loop (make-nbody-opt) 250000))
