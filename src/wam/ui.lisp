(in-package #:bones.wam)


(defparameter *database* nil)

(defmacro with-database (&body body)
  `(let ((*database* (make-wam)))
     ,@body))


(defun add-rules (rules)
  (compile-rules *database* rules))


(defmacro rule (&body body)
  `(add-rules '(,body)))

(defmacro fact (&body body)
  `(add-rules '(,body)))

(defmacro rules (&body rules)
  `(add-rules ',rules))

(defmacro facts (&body rules)
  `(add-rules ',(mapcar #'list rules)))


(defun display-results (results)
  (format t "~%")
  (loop :for (var . result) :in results :do
        (format t "~S = ~S~%" var result)))

(defun result-one (results)
  (display-results results)
  t)

(defun result-all (results)
  (display-results results)
  nil)

(defun result-interactive (results)
  (display-results results)
  (format t "~%More? [Yn] ")
  (force-output)
  (switch ((read-line) :test #'string=)
    ("y" nil)
    ("" nil)
    ("n" t)
    (t t)))


(defun perform-query (query mode)
  (run-query *database* query
             (ecase mode
               (:interactive #'result-interactive)
               (:all #'result-all)
               (:one #'result-one))))


(defmacro query (&body body)
  `(perform-query ',body :interactive))

(defmacro query-all (&body body)
  `(perform-query ',body :all))

(defmacro query-one (&body body)
  `(perform-query ',body :one))


(defun dump (&optional full-code)
  (dump-wam-full *database*)
  (when full-code
    (dump-wam-code *database*)))
