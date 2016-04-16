(in-package #:bones.wam)


(defparameter *database* nil)

(defmacro with-database (&body body)
  `(let ((*database* (make-wam)))
     ,@body))

(defun add-rule (rule)
  (compile-program *database* rule))

(defmacro rule (&body body)
  `(add-rule ',body))

(defun perform-query (query)
  (run-query *database* query))

(defmacro query (&body body)
  `(perform-query ',body))

