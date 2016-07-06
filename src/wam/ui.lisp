(in-package #:bones.wam)


;;;; Database
(defvar *database* nil)


(defun make-database ()
  (make-wam))

(defmacro with-database (database &body body)
  `(let ((*database* ,database))
     ,@body))

(defmacro with-fresh-database (&body body)
  `(with-database (make-database) ,@body))


;;;; Assertion
(defun invoke-rule (head &rest body)
  (wam-logic-frame-add-clause! *database* (list* head body))
  (values))

(defun invoke-fact (fact)
  (invoke-rule fact)
  (values))

(defun invoke-facts (&rest facts)
  (mapc #'add-fact facts)
  (values))


(defmacro rule (head &body body)
  `(invoke-rule ',head ,@(loop :for term :in body :collect `',term)))

(defmacro fact (fact)
  `(invoke-fact ',fact))

(defmacro facts (&body facts)
  `(progn
     ,@(loop :for f :in facts :collect `(fact ,f))))


;;;; Logic Frames
(defun push-logic-frame ()
  (wam-push-logic-frame! *database*))

(defun pop-logic-frame ()
  (wam-pop-logic-frame! *database*))

(defun finalize-logic-frame ()
  (wam-finalize-logic-frame! *database*))

(defmacro push-logic-frame-with (&body body)
  `(prog2
     (push-logic-frame)
     (progn ,@body)
     (finalize-logic-frame)))


;;;; Querying
(defun invoke-query (&rest terms)
  (let ((result nil)
        (succeeded nil))
    (run-query *database* terms
               :result-function (lambda (r)
                                  (setf result r
                                        succeeded t)
                                  t))
    (values result succeeded)))

(defun invoke-query-all (&rest terms)
  (let ((results nil))
    (run-query *database* terms
               :result-function (lambda (result)
                                  (push result results)
                                  nil))
    (nreverse results)))

(defun invoke-query-map (function &rest terms)
  (let ((results nil))
    (run-query *database* terms
               :result-function (lambda (result)
                                  (push (funcall function result) results)
                                  nil))
    (nreverse results)))

(defun invoke-query-do (function &rest terms)
  (run-query *database* terms
             :result-function (lambda (result)
                                (funcall function result)
                                nil))
  (values))

(defun invoke-query-find (predicate &rest terms)
  (let ((results nil)
        (succeeded nil))
    (run-query *database* terms
               :result-function (lambda (result)
                                  (if (funcall predicate result)
                                    (progn (setf results result
                                                 succeeded t)
                                           t)
                                    nil)))
    (values results succeeded)))

(defun invoke-prove (&rest terms)
  (let ((succeeded nil))
    (run-query *database* terms
               :result-function (lambda (result)
                                  (declare (ignore result))
                                  (setf succeeded t)
                                  t))
    succeeded))


(defun quote-terms (terms)
  (loop :for term :in terms :collect `',term))

(defmacro query (&rest terms)
  `(invoke-query ,@(quote-terms terms)))

(defmacro query-all (&rest terms)
  `(invoke-query-all ,@(quote-terms terms)))

(defmacro query-map (function &rest terms)
  `(invoke-query-map ,function ,@(quote-terms terms)))

(defmacro query-do (function &rest terms)
  `(invoke-query-do ,function ,@(quote-terms terms)))

(defmacro query-find (predicate &rest terms)
  `(invoke-query-find ,predicate ,@(quote-terms terms)))

(defmacro prove (&rest terms)
  `(invoke-prove ,@(quote-terms terms)))


;;;; Debugging
(defun dump (&optional full-code)
  (dump-wam-full *database*)
  (when full-code
    (dump-wam-code *database*)))

(defmacro bytecode (&body body)
  `(with-fresh-database
     (push-logic-frame-with ,@body)
     (dump-wam-code *database*)))

