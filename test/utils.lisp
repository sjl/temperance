(in-package #:temperance-test.utils)


;;;; Utils
(defmacro define-test (name &body body)
  "Define a 1am test that uses the correct package.

  This makes the test output less of an unreadable mess.

  "
  `(test ,name
    (let ((*package* ,*package*))
      ,@body)))


(defun result= (x y)
  (set-equal (plist-alist x)
             (plist-alist y)
             :test #'equal))

(defun results= (r1 r2)
  (set-equal r1 r2 :test #'result=))


(defmacro should-fail (&body queries)
  `(progn
     ,@(loop :for query :in queries :collect
             `(is (results= nil (query-all ,query))))))

(defmacro should-return (&body queries)
  `(progn
    ,@(loop :for (query . results) :in queries
            :collect
            `(is (results= ',(cond
                               ((equal results '(empty))
                                (list nil))
                               ((equal results '(fail))
                                nil)
                               (t results))
                           (query-all ,query))))))


;;;; Prolog
(defun %= ()
  (push-logic-frame-with
    (fact (= ?x ?x))))

(defun %not ()
  (push-logic-frame-with
    (rule (not ?x) (call ?x) ! fail)
    (fact (not ?x))))
(defun %append ()
  (push-logic-frame-with
    (fact (append nil ?l ?l))
    (rule (append (list* ?x ?rest) ?l (list* ?x ?result))
      (append ?rest ?l ?result))))

(defun %member ()
  (push-logic-frame-with
    (fact (member ?x (list* ?x ?)))
    (rule (member ?x (list* ? ?rest))
      (member ?x ?rest))))
