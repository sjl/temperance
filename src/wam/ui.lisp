(in-package #:bones.wam)


(defparameter *database* nil)
(defparameter *debug* nil)

(defmacro with-database (&body body)
  `(let ((*database* (make-wam)))
     ,@body))


(defun add-rule (rule)
  (compile-program *database* rule))

(defun perform-query (query)
  (run-query *database* query *debug*))


(defmacro rule (&body body)
  `(add-rule ',body))

(defmacro query (&body body)
  `(perform-query ',body))

(defun dump ()
  (dump-wam-full *database*))
