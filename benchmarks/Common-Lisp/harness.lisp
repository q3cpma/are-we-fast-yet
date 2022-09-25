(require 'asdf)
;; (declaim (optimize (debug 0) (safety 0) (speed 3)))
(load "benchmark")

(defclass run ()
  ((name :accessor name :initarg :name :initform (error "name slot initarg is mandatory"))
   (benchfun :accessor benchfun)
   (total :accessor total :initform 0)
   (num-iterations :accessor num-iterations :initarg :num-iterations)
   (inner-iterations :accessor inner-iterations :initarg :inner-iterations))
  (:default-initargs
   :num-iterations 1
   :inner-iterations 1))

(defmethod initialize-instance :after ((run run) &rest args)
  (declare (ignore args))
  (setf (benchfun run)
		(let ((name-upcase (string-upcase (name run))))
		  (load (name run))
		  (lambda (iter) (funcall #'benchmark:inner-benchmark-loop
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

(defmethod run-benchmark ((run run))
  (format t "Starting ~S benchmark ...~%" (name run))
  (do-runs run)
  (report-benchmark run))

(defmethod measure ((run run))
  (let* ((start (get-internal-real-time))
		 (result (funcall (benchfun run) (inner-iterations run)))
		 (elapsed (/
				   (- (get-internal-real-time) start)
				   internal-time-units-per-second)))
	(when (not result)
	  (error "Benchmark failed with incorrect result"))
	(print-result run elapsed)
	(incf (total run) elapsed)))

(defmethod do-runs ((run run))
  (loop repeat (num-iterations run)
		do (measure run)))

(defmethod report-benchmark ((run run))
  (with-slots (name num-iterations total) run
	(format t "~S: iterations=~D average: ~,0,6Fus total: ~,0,6Fus~%~%"
			name num-iterations (/ total num-iterations) total)))

(defmethod print-result ((run run) run-time)
  (format t "~S: iterations=1 runtime: ~,0,6Fus~%" (name run) run-time))

(defmethod print-total ((run run))
  (format t "Total Runtime: ~,0,6Fus~%" (total run)))


(if (null (uiop:command-line-arguments))
	(error "Arguments: benchmark [num-iterations [inner-iterations]]"))

(let ((run (apply #'make-run (uiop:command-line-arguments))))
  (run-benchmark run)
  (print-total run))
