(in-package #:bones.wam)


(defparameter *database* nil)
(defvar *results* nil)


(defun make-database ()
  (make-wam))

(defmacro with-database (database &body body)
  `(let ((*database* ,database))
     ,@body))

(defmacro with-fresh-database (&body body)
  `(with-database (make-database) ,@body))


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
