(in-package #:temperance.wam)


;;;; Database
(defvar *standard-database* (make-wam))

(defun ensure-database (database-designator)
  (etypecase database-designator
    ((eql t) *standard-database*)
    (wam database-designator)))


(defun make-database ()
  (make-wam))

(defun reset-standard-database ()
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
(defun invoke-rule (database head &rest body)
  (wam-logic-frame-add-clause! (ensure-database database)
                               (list* (normalize-term head)
                                      (mapcar #'normalize-term body)))
  nil)

(defun invoke-fact (database fact)
  (invoke-rule database fact)
  nil)

(defun invoke-facts (database &rest facts)
  (loop :for fact :in facts
        :do (invoke-fact database fact))
  nil)


(defmacro rule (database head &body body)
  `(invoke-rule ,database
                ',head ,@(loop :for term :in body :collect `',term)))

(defmacro fact (database fact)
  `(invoke-fact ,database ',fact))

(defmacro facts (database &body facts)
  (once-only (database)
    `(progn
      ,@(loop :for f :in facts :collect `(fact ,database ,f)))))


;;;; Logic Frames
(defun push-logic-frame (database)
  (wam-push-logic-frame! (ensure-database database)))

(defun pop-logic-frame (database)
  (wam-pop-logic-frame! (ensure-database database)))

(defun finalize-logic-frame (database)
  (wam-finalize-logic-frame! (ensure-database database)))

(defmacro push-logic-frame-with (database &body body)
  (once-only (database)
    `(prog2
      (push-logic-frame ,database)
      (progn ,@body)
      (finalize-logic-frame ,database))))


;;;; Querying
(defun perform-aot-query (database code size vars result-function)
  (run-aot-compiled-query (ensure-database database) code size vars
                          :result-function result-function))

(defun perform-query (database terms result-function)
  (run-query (ensure-database database)
             (mapcar #'normalize-term terms)
             :result-function result-function))


(defmacro define-invocation ((name aot-name) arglist &body body)
  (with-gensyms (code size vars)
    `(progn
      (defun ,name (database ,@arglist &rest terms)
        (macrolet ((invoke (result-function)
                     `(perform-query database terms ,result-function)))
          ,@body))
      (defun ,aot-name (database ,@arglist data)
        (destructuring-bind (,code ,size ,vars) data
          (macrolet ((invoke (result-function)
                       `(perform-aot-query database ,',code ,',size ,',vars
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

(defmacro query (database &rest terms)
  `(invoke-query ,database ,@(quote-terms terms)))

(defmacro query-all (database &rest terms)
  `(invoke-query-all ,database ,@(quote-terms terms)))

(defmacro query-map (database function &rest terms)
  `(invoke-query-map ,database ,function ,@(quote-terms terms)))

(defmacro query-do (database function &rest terms)
  `(invoke-query-do ,database ,function ,@(quote-terms terms)))

(defmacro query-find (database predicate &rest terms)
  `(invoke-query-find ,database ,predicate ,@(quote-terms terms)))

(defmacro prove (database &rest terms)
  `(invoke-prove ,database ,@(quote-terms terms)))


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


(define-invocation-compiler-macro invoke-query invoke-query-aot (database))

(define-invocation-compiler-macro invoke-query-all invoke-query-all-aot (database))

(define-invocation-compiler-macro invoke-query-map invoke-query-map-aot (database function))

(define-invocation-compiler-macro invoke-query-do invoke-query-do-aot (database function))

(define-invocation-compiler-macro invoke-query-find invoke-query-find-aot (database predicate))

(define-invocation-compiler-macro invoke-prove invoke-prove-aot (database))


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

