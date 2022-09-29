(declaim (optimize (debug 0) (safety 0) (speed 3)))
(load (make-pathname :directory (pathname-directory *load-truename*) :name "uiop-minimal"))

(defclass run ()
  ((name :accessor name :initarg :name :initform (error "name slot initarg is mandatory"))
   (benchfun :accessor benchfun)
   (total :accessor total :initform 0)
   (num-iterations :accessor num-iterations :initarg :num-iterations)
   (inner-iterations :accessor inner-iterations :initarg :inner-iterations))
  (:default-initargs
   :num-iterations 1
   :inner-iterations 1))

(defmethod initialize-instance :after ((self run) &rest args)
  (declare (ignore args))
  (setf (benchfun self)
		(let ((name-upcase (string-upcase (name self))))
		  (load (name self))
		  (lambda (iter) (funcall (symbol-function (find-symbol "INNER-BENCHMARK-LOOP" 'benchmark))
								  (make-instance
								   (find-symbol name-upcase (make-symbol name-upcase)))
								  iter)))))

(defun make-run (name &optional num-iterations inner-iterations)
  (apply #'make-instance `(run :name ,(string-downcase name)
							   ,@(when num-iterations `(:num-iterations
														,(if (integerp num-iterations)
															 num-iterations
															 (parse-integer num-iterations))))
							   ,@(when inner-iterations `(:inner-iterations
														  ,(if (integerp inner-iterations)
															   inner-iterations
															   (parse-integer inner-iterations)))))))

(defmethod run-benchmark ((self run))
  (format t "Starting ~S benchmark ...~%" (name self))
  (do-runs self)
  (report-benchmark self))

(defmethod measure ((self run))
  (let* ((start (get-internal-real-time))
		 (result (funcall (benchfun self) (inner-iterations self)))
		 (elapsed (/
				   (- (get-internal-real-time) start)
				   internal-time-units-per-second)))
	(when (not result)
	  (uiop:die 1 "Benchmark failed with incorrect result"))
	(print-result self elapsed)
	(incf (total self) elapsed)))

(defmethod do-runs ((self run))
  (loop repeat (num-iterations self)
		do (measure self)))

(defmethod report-benchmark ((self run))
  (with-slots (name num-iterations total) self
	(format t "~S: iterations=~D average: ~,0,6Fus total: ~,0,6Fus~%~%"
			name num-iterations (/ total num-iterations) total)))

(defmethod print-result ((self run) run-time)
  (format t "~S: iterations=1 runtime: ~,0,6Fus~%" (name self) run-time))

(defmethod print-total ((self run))
  (format t "Total Runtime: ~,0,6Fus~%" (total self)))



(if (null (uiop:command-line-arguments))
	(error "Arguments: benchmark [num-iterations [inner-iterations]]"))

(let ((run (apply #'make-run (uiop:command-line-arguments))))
  (run-benchmark run)
  (print-total run))
