(in-package :temperance)


;;;; Database -----------------------------------------------------------------
(defvar *standard-database* (make-wam)
  "The standard database used when `t` is supplied as a database designator.")

(defun ensure-database (database-designator)
  (etypecase database-designator
    ((eql t) *standard-database*)
    (wam database-designator)))


(defun make-database ()
  "Create and return a fresh database."
  (make-wam))

(defun reset-standard-database ()
  "Reset `*standard-database*` to a new, fresh database."
  (setf *standard-database* (make-database)))


(defmacro with-database (database &body body)
  "Execute `body` with `*standard-database*` bound to `database`."
  `(let ((*standard-database* ,database))
     ,@body))

(defmacro with-fresh-database (&body body)
  "Execute `body` with `*standard-database*` bound to a new, fresh database."
  `(with-database (make-database) ,@body))


;;;; Normalization ------------------------------------------------------------
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


;;;; Assertion ----------------------------------------------------------------
(defun invoke-rule (database head &rest body)
  "Add a logical rule to `database` with the given `head` and `body`.

  The `rule` macro is a nicer interface, but this function can be useful if you
  need to build rules dynamically at runtime.

  Example:

    ; Sally like anyone who likes cats
    (invoke-rule t '(likes sally ?who)
      '(likes ?who cats))

  "
  (wam-logic-frame-add-clause! (ensure-database database)
                               (list* (normalize-term head)
                                      (mapcar #'normalize-term body)))
  nil)

(defun invoke-fact (database fact)
  "Add a logical fact to `database`.

  The `fact` macro is a nicer interface, but this function can be useful if you
  need to build rules dynamically at runtime.

  Examples:

    (invoke-fact t '(successor 0 1))

    (defun add-cat-lover (name)
      (invoke-fact t `(likes ,name cats)))

  "
  (invoke-rule database fact)
  nil)

(defun invoke-facts (database &rest facts)
  "Add zero or more logical facts to `database`.

  The `facts` macro is a nicer interface, but this function can be useful if you
  need to build rules dynamically at runtime.

  Examples:

    (invoke-facts t
                  '(successor 0 1)
                  '(successor 1 2)
                  '(successor 2 3))

  "
  (loop :for fact :in facts
        :do (invoke-fact database fact))
  nil)


(defmacro rule (database head &body body)
  "Add a logical rule to `database` with the given `head` and `body`.

  `head` and `body` will be wrapped in `(quote ...)`.  If you need to
  dynamically construct rules at runtime, see `invoke-rule`.

  Example:

    ; Sally like anyone who likes cats
    (rule t (likes sally ?who)
      (likes ?who cats))

  "

  `(invoke-rule ,database
                ',head ,@(loop :for term :in body :collect `',term)))

(defmacro fact (database fact)
  "Add a logical fact to `database`.

  `fact` will be wrapped in `(quote ...)`.  If you need to dynamically construct
  facts at runtime, see `invoke-fact`.

  Examples:

    (fact t (likes kim cats))
    (fact t (likes sjl cats))

  "
  `(invoke-fact ,database ',fact))

(defmacro facts (database &body facts)
  "Add zero or more logical facts to `database`.

  Each fact in `facts` will be wrapped in `(quote ...)`.  If you need to
  dynamically construct facts at runtime, see `invoke-facts`.

  Examples:

    (facts t
      (successor 0 1)
      (successor 1 2)
      (successor 2 3))

  "
  (once-only (database)
    `(progn
      ,@(loop :for f :in facts :collect `(fact ,database ,f)))))


;;;; Logic Frames -------------------------------------------------------------
(defun push-logic-frame (database)
  "Push a new, open logic frame onto `database`.

  An error will be signaled if there is already an unfinalized logic frame on
  the top of the stack.

  "
  (wam-push-logic-frame! (ensure-database database)))

(defun pop-logic-frame (database)
  "Pop off the top logic frame of `database`'s logic stack.

  An error will be signaled if the logic stack is empty or the top frame is
  unfinalized.

  "
  (wam-pop-logic-frame! (ensure-database database)))

(defun finalize-logic-frame (database)
  "Finalize the top logic frame of `database`'s logic stack.

  An error will be signaled if the logic stack is empty or the top frame is
  already finalized.

  "
  (wam-finalize-logic-frame! (ensure-database database)))

(defmacro push-logic-frame-with (database &body body)
  "Push a new logic frame onto `database`, run `body`, and finalize it.

  This is a convenience macro for the common process of pushing a logic frame,
  adding some stuff to it, and finalizing it right away.

  Example:

    (push-logic-frame-with t
      (rule t (likes sally ?who)
        (likes ?who cats))
      (facts t
        (likes kim cats)
        (likes sjl cats)
        (likes bob dogs)))

    (query-all t (likes sally ?who))
    ; =>
    ((?who kim) (?who sjl))

  "
  (once-only (database)
    `(prog2
      (push-logic-frame ,database)
      (progn ,@body)
      (finalize-logic-frame ,database))))


;;;; Querying -----------------------------------------------------------------
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

(define-invocation (invoke-query-for invoke-query-for-aot) (variable)
  (let ((results nil))
    (invoke (lambda (result)
              (push (getf result variable) results)
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

(defmacro query-for (database variable &rest terms)
  `(invoke-query-for ,database ',variable ,@(quote-terms terms)))

(defmacro query-do (database function &rest terms)
  `(invoke-query-do ,database ,function ,@(quote-terms terms)))

(defmacro query-find (database predicate &rest terms)
  `(invoke-query-find ,database ,predicate ,@(quote-terms terms)))

(defmacro prove (database &rest terms)
  `(invoke-prove ,database ,@(quote-terms terms)))


;;;; Chili Dogs ---------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-aot-data-form (terms)
    (with-gensyms (code size vars)
      `(load-time-value
        (let* ((,code (allocate-query-holder)))
          (multiple-value-bind (,vars ,size)
              (compile-query-into
                ,code ',(-<> terms
                          (mapcar #'eval <>)
                          (mapcar #'normalize-term <>)))
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

(define-invocation-compiler-macro invoke-query-for invoke-query-for-aot (database variable))

(define-invocation-compiler-macro invoke-query-do invoke-query-do-aot (database function))

(define-invocation-compiler-macro invoke-query-find invoke-query-find-aot (database predicate))

(define-invocation-compiler-macro invoke-prove invoke-prove-aot (database))


;;;; Debugging ----------------------------------------------------------------
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

(defmacro trace-predicate (functor)
  `(pushnew ',functor *trace*))

(defmacro untrace-predicate (functor)
  `(setf *trace* (remove ',functor *trace*)))

(defun untrace-all ()
  (setf *trace* nil))

