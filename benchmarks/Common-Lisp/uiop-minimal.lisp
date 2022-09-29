(defpackage #:uiop
  (:use #:cl)
  (:export
   #:command-line-arguments
   #:quit
   #:die))
(in-package #:uiop)

(defvar *default-stream-element-type*
  (or #+(or abcl cmucl cormanlisp scl xcl) 'character
	  #+lispworks 'lw:simple-char
	  :default)
  "default element-type for open (depends on the current CL implementation)")

(defvar *stdin* *standard-input*
  "the original standard input stream at startup")

(defun setup-stdin ()
  (setf *stdin*
		#.(or #+clozure 'ccl::*stdin*
			  #+(or cmucl scl) 'system:*stdin*
			  #+(or clasp ecl) 'ext::+process-standard-input+
			  #+sbcl 'sb-sys:*stdin*
			  '*standard-input*)))

(defvar *stdout* *standard-output*
  "the original standard output stream at startup")

(defun setup-stdout ()
  (setf *stdout*
		#.(or #+clozure 'ccl::*stdout*
			  #+(or cmucl scl) 'system:*stdout*
			  #+(or clasp ecl) 'ext::+process-standard-output+
			  #+sbcl 'sb-sys:*stdout*
			  '*standard-output*)))

(defvar *stderr* *error-output*
  "the original error output stream at startup")

(defun setup-stderr ()
  (setf *stderr*
		#.(or #+allegro 'excl::*stderr*
			  #+clozure 'ccl::*stderr*
			  #+(or cmucl scl) 'system:*stderr*
			  #+(or clasp ecl) 'ext::+process-error-output+
			  #+sbcl 'sb-sys:*stderr*
			  '*error-output*)))

;; Run them now. In image.lisp, we'll register them to be run at image restart.
(setup-stdin) (setup-stdout) (setup-stderr)

(defmacro with-safe-io-syntax ((&key (package :cl)) &body body)
  "Establish safe CL reader options around the evaluation of BODY"
  `(call-with-safe-io-syntax #'(lambda () (let ((*package* (find-package ,package))) ,@body))))

(defun call-with-safe-io-syntax (thunk &key (package :cl))
  (with-standard-io-syntax
   (let ((*package* (find-package package))
         (*read-default-float-format* 'double-float)
         (*print-readably* nil)
         (*read-eval* nil))
     (funcall thunk))))

(defun finish-outputs (&rest streams)
  "Finish output on the main output streams as well as any specified one.
Useful for portably flushing I/O before user input or program exit."
  ;; CCL notably buffers its stream output by default.
  (dolist (s (append streams
                     (list *stdout* *stderr* *error-output* *standard-output* *trace-output*
                           *debug-io* *terminal-io* *query-io*)))
    (ignore-errors (finish-output s)))
  (values))

(defun format! (stream format &rest args)
  "Just like format, but call finish-outputs before and after the output."
  (finish-outputs stream)
  (apply 'format stream format args)
  (finish-outputs stream))

(defun safe-format! (stream format &rest args)
  "Variant of FORMAT that is safe against both
dangerous syntax configuration and errors while printing."
  (with-safe-io-syntax ()
					   (ignore-errors (apply 'format! stream format args))
					   (finish-outputs stream))) ; just in case format failed

(eval-when (:load-toplevel :compile-toplevel :execute)
  (deftype package-designator () '(and (or package character string symbol) (satisfies find-package)))
  (define-condition no-such-package-error (type-error)
    ()
    (:default-initargs :expected-type 'package-designator)
    (:report (lambda (c s)
			   (format s "No package named ~a" (string (type-error-datum c))))))

  (defmethod package-designator ((c no-such-package-error))
    (type-error-datum c))

  (defun find-package* (package-designator &optional (errorp t))
    "Like CL:FIND-PACKAGE, but by default raises a UIOP:NO-SUCH-PACKAGE-ERROR if the
  package is not found."
    (let ((package (find-package package-designator)))
      (cond
        (package package)
        (errorp (error 'no-such-package-error :datum package-designator))
        (t nil))))

  (defun find-symbol* (name package-designator &optional (error t))
    "Find a symbol in a package of given string'ified NAME;
unlike CL:FIND-SYMBOL, work well with 'modern' case sensitive syntax
by letting you supply a symbol or keyword for the name;
also works well when the package is not present.
If optional ERROR argument is NIL, return NIL instead of an error
when the symbol is not found."
    (block nil
      (let ((package (find-package* package-designator error)))
        (when package ;; package error handled by find-package* already
          (multiple-value-bind (symbol status) (find-symbol (string name) package)
            (cond
              (status (return (values symbol status)))
              (error (error "There is no symbol ~S in package ~S" name (package-name package))))))
        (values nil nil)))))


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

(defvar *image-dumped-p* nil ; may matter as to how to get to command-line-arguments
  "Is this a dumped image? As a standalone executable?")

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

(defun quit (&optional (code 0) (finish-output t))
  "Quits from the Lisp world, with the given exit status if provided.
This is designed to abstract away the implementation specific quit forms."
  (when finish-output ;; essential, for ClozureCL, and for standard compliance.
    (finish-outputs))
  #+(or abcl xcl) (ext:quit :status code)
  #+allegro (excl:exit code :quiet t)
  #+(or clasp ecl) (si:quit code)
  #+clisp (ext:quit code)
  #+clozure (ccl:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+(or cmucl scl) (unix:unix-exit code)
  #+gcl (system:quit code)
  #+genera (error "~S: You probably don't want to Halt Genera. (code: ~S)" 'quit code)
  #+lispworks (lispworks:quit :status code :confirm nil :return nil :ignore-errors-p t)
  #+mcl (progn code (ccl:quit)) ;; or should we use FFI to call libc's exit(3) ?
  #+mkcl (mk-ext:quit :exit-code code)
  #+sbcl #.(let ((exit (find-symbol* :exit :sb-ext nil))
                 (quit (find-symbol* :quit :sb-ext nil)))
             (cond
			   (exit `(,exit :code code :abort (not finish-output)))
			   (quit `(,quit :unix-status code :recklessly-p (not finish-output)))))
  #-(or abcl allegro clasp clisp clozure cmucl ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
  (not-implemented-error 'quit "(called with exit code ~S)" code))

(defun die (code format &rest arguments)
  "Die in error with some error message"
  (with-safe-io-syntax ()
	(ignore-errors
	 (format! *stderr* "~&~?~&" format arguments)))
  (quit code))
