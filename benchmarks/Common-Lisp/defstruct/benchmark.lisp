(defpackage #:benchmark
  (:use #:cl)
  (:export #:benchmark #:verify-result #:inner-benchmark-loop))
(in-package #:benchmark)


(defstruct benchmark)

(defgeneric benchmark (obj))

(defgeneric verify-result (obj result))

(defmethod inner-benchmark-loop ((bm benchmark) iterations)
  (loop repeat iterations
		always (verify-result bm (benchmark bm))))
