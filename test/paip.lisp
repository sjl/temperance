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
  (not-unifies () (a))
  )

(test variable-unification
  (unifies :x 1 ((:x . 1)))
  (unifies :x 2 ((:x . 2)))
  (unifies :x a ((:x . a)))
  (unifies :x :y ((:x . :y)))
  (unifies (likes sally :thing)
           (likes :person cats)
           ((:thing . cats)
            (:person . sally)))
  (unifies (:x + :y)
           (10 + (1 + 2))
           ((:x . 10)
            (:y . (1 + 2))))
  (unifies (:x + (:y + :z))
           (10 + (1 + 2))
           ((:x . 10)
            (:y . 1)
            (:z . 2))))

(test occurs-unification
  (not-unifies :x (f :x))
  (not-unifies :x (f (:x 1)))
  (not-unifies :x (:x :x))
  (not-unifies :x (:x :y))
  (let ((*check-occurs* nil))
    (unifies :x (f :x)     ((:x . (f :x))))
    (unifies :x (f (:x 1)) ((:x . (f (:x 1)))))
    (unifies :x (:x :x)    ((:x . (:x :x))))
    (unifies :x (:x :y)    ((:x . (:x :y))))))
