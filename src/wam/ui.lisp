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


;;;; Normalization
(defun normalize-term (term)
  ;; Normally a rule consists of a head terms and multiple body terms, like so:
  ;;
  ;;     (likes sally ?who) (likes ?who cats)
  ;;
  ;; But sometimes people are lazy and don't include the parens around
  ;; zero-arity predicates:
  ;;
  ;;     (happy steve) sunny
  (if (and (not (variablep term))
           (symbolp term)
           (not (eq term '!))) ; jesus
    (list term)
    term))


;;;; Assertion
(defun invoke-rule (head &rest body)
  (assert *database* (*database*) "No database.")
  (wam-logic-frame-add-clause! *database*
                               (list* (normalize-term head)
                                      (mapcar #'normalize-term body)))
  (values))

(defun invoke-fact (fact)
  (invoke-rule fact)
  (values))

(defun invoke-facts (&rest facts)
  (mapc #'invoke-fact facts)
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
  (assert *database* (*database*) "No database.")
  (wam-push-logic-frame! *database*))

(defun pop-logic-frame ()
  (assert *database* (*database*) "No database.")
  (wam-pop-logic-frame! *database*))

(defun finalize-logic-frame ()
  (assert *database* (*database*) "No database.")
  (wam-finalize-logic-frame! *database*))

(defmacro push-logic-frame-with (&body body)
  `(prog2
     (push-logic-frame)
     (progn ,@body)
     (finalize-logic-frame)))


;;;; Querying
(defun perform-query (terms result-function)
  (assert *database* (*database*) "No database.")
  (run-query *database* (mapcar #'normalize-term terms)
             :result-function result-function))


(defun invoke-query (&rest terms)
  (let ((result nil)
        (succeeded nil))
    (perform-query terms (lambda (r)
                           (setf result r
                                 succeeded t)
                           t))
    (values result succeeded)))

(defun invoke-query-all (&rest terms)
  (let ((results nil))
    (perform-query terms (lambda (result)
                           (push result results)
                           nil))
    (nreverse results)))

(defun invoke-query-map (function &rest terms)
  (let ((results nil))
    (perform-query terms (lambda (result)
                           (push (funcall function result) results)
                           nil))
    (nreverse results)))

(defun invoke-query-do (function &rest terms)
  (perform-query terms (lambda (result)
                         (funcall function result)
                         nil))
  (values))

(defun invoke-query-find (predicate &rest terms)
  (let ((results nil)
        (succeeded nil))
    (perform-query terms (lambda (result)
                           (if (funcall predicate result)
                             (progn (setf results result
                                          succeeded t)
                                    t)
                             nil)))
    (values results succeeded)))

(defun invoke-prove (&rest terms)
  (let ((succeeded nil))
    (perform-query terms (lambda (result)
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

