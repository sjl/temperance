(in-package #:bones-test.paip)

(def-suite :bones.paip)
(in-suite :bones.paip)

;;;; Utils
(defun alist-equal (x y)
  (set-equal x y :test #'equal))

(defmacro unifies (x y bindings)
  `(is (alist-equal ,(if (eql bindings 'no-bindings)
                       'no-bindings
                       `',bindings)
                    (unify ',x ',y))))

(defmacro not-unifies (x y)
  `(is (eql bones.paip:fail (unify ',x ',y))))


(defmacro with-db (rules &rest body)
  `(progn
    (clear-db)
    ,@(mapcar (lambda (rule) `(rule ,@rule))
              rules)
    ,@body))


(defmacro proves (query)
  `(is-true (return-all ,query)))

(defmacro not-proves (query)
  `(is-false (return-all ,query)))

(defmacro proves-with (query results)
  `(is (set-equal ',results (return-all ,query)
                  :test #'alist-equal)))


;;;; Unification
(test constant-unification
  (unifies 1 1 no-bindings)
  (unifies foo foo no-bindings)
  (unifies (a) (a) no-bindings)
  (unifies (a b c) (a b c) no-bindings)
  (not-unifies 1 2)
  (not-unifies foo bar)
  (not-unifies a (a))
  (not-unifies (a) (a b))
  (not-unifies () (a)))

(test variable-unification
  (unifies ?x 1 ((?x . 1)))
  (unifies ?x a ((?x . a)))
  (unifies ?x ?y ((?x . ?y)))
  (unifies (?x (f ?x)) (2 (f 2)) ((?x . 2)))
  (unifies (likes sally ?thing)
           (likes ?person cats)
           ((?thing . cats)
            (?person . sally)))
  (unifies (?x + ?y)
           (10 + (1 + 2))
           ((?x . 10)
            (?y . (1 + 2))))
  (unifies (?x + (?y + ?z))
           (10 + (1 + 2))
           ((?x . 10)
            (?y . 1)
            (?z . 2)))
  (not-unifies (?x ?x) (1 2))
  (not-unifies (?x ?y ?x) (1 1 3)))

(test occurs-unification
  (not-unifies ?x (f ?x))
  (not-unifies ?x (f (?x 1)))
  (not-unifies ?x (?x ?x))
  (not-unifies ?x (?x ?y))
  (let ((*check-occurs* nil))
    (unifies ?x (f ?x)     ((?x . (f ?x))))
    (unifies ?x (f (?x 1)) ((?x . (f (?x 1)))))
    (unifies ?x (?x ?x)    ((?x . (?x ?x))))
    (unifies ?x (?x ?y)    ((?x . (?x ?y))))))


;;;; Basic Proving
(test prove-facts
  (with-db (((likes kim cats))
            ((likes tom cats)))
    (proves (likes kim cats))
    (proves (likes tom cats))
    (not-proves (likes kim tom))
    (not-proves (likes kim))))

(test prove-rules-simple
  (with-db (((likes kim cats))
            ((likes sally ?x) (likes ?x cats)))
    (proves (likes sally kim))
    (not-proves (likes sally sally))))

(test prove-member
  (with-db (((member ?x (?x . ?tail)))
            ((member ?x (?y . ?tail)) (member ?x ?tail)))
    (proves (member 1 (1 2)))
    (proves (member 2 (1 2)))
    (proves (member (x) (1 (x) 2)))
    (not-proves (member 1 ()))
    (not-proves (member ?x ()))
    (not-proves (member 1 (a b c)))
    (proves-with (member ?x (1 2 3))
                 (((?x . 1))
                  ((?x . 2))
                  ((?x . 3))))
    (proves-with (member ?x (1 1 1))
                 (((?x . 1))))
    (proves-with (member (?x ?y) ((a b) (c d) 1))
                 (((?x . a) (?y . b))
                  ((?x . c) (?y . d))))))

(test prove-last
  (with-db (((last ?x (?x)))
            ((last ?x (?y . ?tail)) (last ?x ?tail)))
    (proves (last 1 (1)))
    (proves (last 2 (1 2)))
    (not-proves (last 1 ()))
    (not-proves (last 1 ((1))))
    (not-proves (last 1 (1 2)))
    (proves-with (last ?x (1 2 3))
                 (((?x . 3))))
    (proves-with (last ?x (1 (2 3)))
                 (((?x . (2 3)))))))
