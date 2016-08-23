(in-package #:temperance.wam)


;;;; Database
(defvar *standard-database* nil)


(defun make-database ()
  (make-wam))

(defun reset-database ()
  (setf *standard-database* (make-database)))


(defmacro with-database (database &body body)
  `(let ((*standard-database* ,database))
     ,@body))

(defmacro with-fresh-database (&body body)
  `(with-database (make-database) ,@body))


;;;; Normalization
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun normalize-term (term)
    ;; Normally a rule consists of a head terms and many body terms, like so:
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
      term)))


;;;; Assertion
(defun invoke-rule (head &rest body)
  (assert *standard-database* (*standard-database*) "No database.")
  (wam-logic-frame-add-clause! *standard-database*
                               (list* (normalize-term head)
                                      (mapcar #'normalize-term body)))
  nil)

(defun invoke-fact (fact)
  (invoke-rule fact)
  nil)

(defun invoke-facts (&rest facts)
  (mapc #'invoke-fact facts)
  nil)


(defmacro rule (head &body body)
  `(invoke-rule ',head ,@(loop :for term :in body :collect `',term)))

(defmacro fact (fact)
  `(invoke-fact ',fact))

(defmacro facts (&body facts)
  `(progn
     ,@(loop :for f :in facts :collect `(fact ,f))))


;;;; Logic Frames
(defun push-logic-frame ()
  (assert *standard-database* (*standard-database*) "No database.")
  (wam-push-logic-frame! *standard-database*))

(defun pop-logic-frame ()
  (assert *standard-database* (*standard-database*) "No database.")
  (wam-pop-logic-frame! *standard-database*))

(defun finalize-logic-frame ()
  (assert *standard-database* (*standard-database*) "No database.")
  (wam-finalize-logic-frame! *standard-database*))

(defmacro push-logic-frame-with (&body body)
  `(prog2
     (push-logic-frame)
     (progn ,@body)
     (finalize-logic-frame)))


;;;; Querying
(defun perform-aot-query (code size vars result-function)
  (assert *standard-database* (*standard-database*) "No database.")
  (run-aot-compiled-query *standard-database* code size vars
                          :result-function result-function))

(defun perform-query (terms result-function)
  (assert *standard-database* (*standard-database*) "No database.")
  (run-query *standard-database* (mapcar #'normalize-term terms)
             :result-function result-function))


(defmacro define-invocation ((name aot-name) arglist &body body)
  (with-gensyms (terms data code size vars)
    `(progn
      (defun ,name ,(append arglist `(&rest ,terms))
        (macrolet ((invoke (result-function)
                     `(perform-query ,',terms ,result-function)))
          ,@body))
      (defun ,aot-name ,(append arglist `(,data))
        (destructuring-bind (,code ,size ,vars) ,data
          (macrolet ((invoke (result-function)
                       `(perform-aot-query ,',code ,',size ,',vars
                                           ,result-function)))
            ,@body))))))


(define-invocation (invoke-query invoke-query-aot) ()
  (let ((result nil)
        (succeeded nil))
    (invoke (lambda (r)
              (setf result r
                    succeeded t)
              t))
    (values result succeeded)))

(define-invocation (invoke-query-all invoke-query-all-aot) ()
  (let ((results nil))
    (invoke (lambda (result)
              (push result results)
              nil))
    (nreverse results)))

(define-invocation (invoke-query-map invoke-query-map-aot) (function)
  (let ((results nil))
    (invoke (lambda (result)
              (push (funcall function result) results)
              nil))
    (nreverse results)))

(define-invocation (invoke-query-do invoke-query-do-aot) (function)
  (invoke (lambda (result)
            (funcall function result)
            nil))
  nil)

(define-invocation (invoke-query-find invoke-query-find-aot) (predicate)
  (let ((results nil)
        (succeeded nil))
    (invoke (lambda (result)
              (if (funcall predicate result)
                (progn (setf results result
                             succeeded t)
                       t)
                nil)))
    (values results succeeded)))

(define-invocation (invoke-prove invoke-prove-aot) ()
  (let ((succeeded nil))
    (invoke (lambda (result)
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


;;;; Chili Dogs
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-aot-data-form (terms)
    (with-gensyms (code size vars)
      `(load-time-value
        (let* ((,code (allocate-query-holder)))
          (multiple-value-bind (,vars ,size)
              (compile-query-into
                ,code ',(->> terms
                          (mapcar #'eval)
                          (mapcar #'normalize-term)))
            (list ,code ,size ,vars)))
        t))))


(defmacro define-invocation-compiler-macro (name aot-name arglist)
  `(define-compiler-macro ,name (&whole form
                                 ,@arglist
                                 &rest terms
                                 &environment env)
    (if (every (rcurry #'constantp env) terms)
      `(,',aot-name ,,@arglist ,(make-aot-data-form terms))
      form)))


(define-invocation-compiler-macro invoke-query      invoke-query-aot ())
(define-invocation-compiler-macro invoke-query-all  invoke-query-all-aot ())
(define-invocation-compiler-macro invoke-query-map  invoke-query-map-aot (function))
(define-invocation-compiler-macro invoke-query-do   invoke-query-do-aot (function))
(define-invocation-compiler-macro invoke-query-find invoke-query-find-aot (predicate))
(define-invocation-compiler-macro invoke-prove      invoke-prove-aot ())


;;;; Debugging
(defun dump (&optional full-code)
  (dump-wam-full *standard-database*)
  (when full-code
    (dump-wam-code *standard-database*)))

(defmacro bytecode (&body body)
  `(with-fresh-database
    (push-logic-frame-with ,@body)
    (format t ";;;; PROGRAM CODE =======================~%")
    (dump-wam-code *standard-database*)
    (format t "~%;;;; QUERY CODE =========================~%")
    (dump-wam-query-code *standard-database*)))

