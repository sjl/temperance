(in-package #:bones.wam)


(defparameter *database* nil)

(defmacro with-database (&body body)
  `(let ((*database* (make-wam)))
     ,@body))


(defun add-rule (rule)
  (compile-program *database* rule))

(defun perform-query (query step)
  (run-query *database* query step))


(defmacro rule (&body body)
  `(add-rule ',body))

(defmacro query (&body body)
  `(perform-query ',body nil))

(defmacro query-step (&body body)
  `(perform-query ',body t))

(defun dump ()
  (dump-wam-full *database*))
