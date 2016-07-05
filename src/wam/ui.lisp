(in-package #:bones.wam)


;;;; Database
(defparameter *database* nil)
(defvar *results* nil)


(defun make-database ()
  (make-wam))

(defmacro with-database (database &body body)
  `(let ((*database* ,database))
     ,@body))

(defmacro with-fresh-database (&body body)
  `(with-database (make-database) ,@body))


;;;; Assertion
(defun add-rule (clause)
  (wam-code-add-clause! *database* clause)
  (values))

(defun add-fact (fact)
  (add-rule (list fact))
  (values))

(defun add-facts (facts)
  (mapc #'add-fact facts)
  (values))


(defmacro rule (&body body)
  `(add-rule ',body))

(defmacro fact (fact)
  `(add-fact ',fact))

(defmacro facts (&body facts)
  `(progn
     ,@(loop :for f :in facts :collect `(fact ,f))))


;;;; Logic Frames
(defun push-logic-frame ()
  (wam-code-push-frame! *database*))

(defun pop-logic-frame ()
  (wam-code-pop-frame! *database*))

(defun finalize-logic-frame ()
  (wam-code-finalize-frame! *database*))

(defmacro push-logic-frame-with (&body body)
  `(prog2
     (push-logic-frame)
     (progn ,@body)
     (finalize-logic-frame)))


;;;; Querying
(defun display-results (results)
  (format t "~%")
  (loop :for (var result . more) :on results :by #'cddr :do
        (format t "~S = ~S~%" var result)))

(defun display-results-one (results)
  (display-results results)
  t)

(defun display-results-all (results)
  (display-results results)
  nil)

(defun display-results-interactive (results)
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
             :result-function
             (ecase mode
               (:interactive #'display-results-interactive)
               (:all #'display-results-all)
               (:one #'display-results-one))
             :status-function
             (lambda (failp)
               (if failp
                 (princ "No.")
                 (princ "Yes."))))
  (values))


(defun return-results-one (results)
  (setf *results* results)
  t)

(defun return-results-all (results)
  (push results *results*)
  nil)


(defun perform-return (query mode)
  (ecase mode
    (:all (let ((*results* nil))
            (run-query *database* query
                       :result-function
                       #'return-results-all)
            (values *results* (ensure-boolean *results*))))
    (:one (let* ((no-results (gensym))
                 (*results* no-results))
            (run-query *database* query
                       :result-function
                       #'return-results-one)
            (if (eql *results* no-results)
              (values nil nil)
              (values *results* t))))))


(defun perform-prove (query)
  (nth-value 1 (perform-return query :one)))


(defmacro query (&body body)
  `(perform-query ',body :interactive))

(defmacro query-all (&body body)
  `(perform-query ',body :all))

(defmacro query-one (&body body)
  `(perform-query ',body :one))


(defmacro return-all (&body body)
  `(perform-return ',body :all))

(defmacro return-one (&body body)
  `(perform-return ',body :one))

(defmacro prove (&body body)
  `(perform-prove ',body))


(defun dump (&optional full-code)
  (dump-wam-full *database*)
  (when full-code
    (dump-wam-code *database*)))
