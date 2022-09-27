(declaim (optimize (debug 0) (safety 0) (speed 3)))

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
	  (error "Benchmark failed with incorrect result"))
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



;;; Proper command-line arguments (from UIOP)
(defun raw-command-line-arguments ()
  "Find what the actual command line for this process was."
  #+abcl ext:*command-line-argument-list* ; Use 1.0.0 or later!
  #+allegro (sys:command-line-arguments) ; default: :application t
  #+(or clasp ecl) (loop :for i :from 0 :below (si:argc) :collect (si:argv i))
  #+clisp (coerce (ext:argv) 'list)
  #+clozure ccl:*command-line-argument-list*
  #+(or cmucl scl) extensions:*command-line-strings*
  #+gcl si:*command-args*
  #+(or genera mcl mezzano) nil
  #+lispworks sys:*line-arguments-list*
  #+mkcl (loop :for i :from 0 :below (mkcl:argc) :collect (mkcl:argv i))
  #+sbcl sb-ext:*posix-argv*
  #+xcl system:*argv*
  #-(or abcl allegro clasp clisp clozure cmucl ecl gcl genera lispworks mcl mezzano mkcl sbcl scl xcl)
  (not-implemented-error 'raw-command-line-arguments))

(defvar *image-dumped-p* nil)
(defun command-line-arguments (&optional (arguments (raw-command-line-arguments)))
  "Extract user arguments from command-line invocation of current process.
Assume the calling conventions of a generated script that uses --
if we are not called from a directly executable image."
  (block nil
	#+abcl (return arguments)
	;; SBCL and Allegro already separate user arguments from implementation arguments.
	#-(or sbcl allegro)
	(unless (eq *image-dumped-p* :executable)
	  ;; LispWorks command-line processing isn't transparent to the user
	  ;; unless you create a standalone executable; in that case,
	  ;; we rely on cl-launch or some other script to set the arguments for us.
	  #+lispworks (return *command-line-arguments*)
	  ;; On other implementations, on non-standalone executables,
	  ;; we trust cl-launch or whichever script starts the program
	  ;; to use -- as a delimiter between implementation arguments and user arguments.
	  #-lispworks (setf arguments (member "--" arguments :test 'string-equal)))
	(rest arguments)))


(if (null (command-line-arguments))
	(error "Arguments: benchmark [num-iterations [inner-iterations]]"))

(let ((run (apply #'make-run (command-line-arguments))))
  (run-benchmark run)
  (print-total run))
