(in-package #:bones.wam)


(defparameter *database* nil)

(defmacro with-database (&body body)
  `(let ((*database* (make-wam)))
     ,@body))



(defun add-rules (rules)
  (compile-rules *database* rules))

(defun perform-query (query step)
  (run-query *database* query step))


(defmacro rule (&body body)
  `(add-rules '(,body)))

(defmacro rules (&body rules)
  `(add-rules ',rules))

(defmacro query (&body body)
  `(perform-query ',body nil))

(defmacro query-step (&body body)
  `(perform-query ',body t))


(defun dump (&optional full-code)
  (dump-wam-full *database*)
  (when full-code
    (dump-wam-code *database*)))
