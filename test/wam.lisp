(in-package #:bones-test.wam)

(def-suite :bones.wam)
(in-suite :bones.wam)


;;;; Setup
(defun make-test-database ()
  (let ((db (make-database)))
    (with-database db
      (push-logic-frame-with

        (facts (always)

               (drinks tom ?anything)
               (drinks kim water)
               (drinks alice bourbon)
               (drinks bob genny-cream)
               (drinks candace birch-beer)

               (listens alice blues)
               (listens alice jazz)
               (listens bob blues)
               (listens bob rock)
               (listens candace blues)

               (fuzzy cats)

               (cute cats)
               (cute snakes))

        (rule (pets alice ?what)
          (cute ?what))

        (rule (pets bob ?what)
          (cute ?what)
          (fuzzy ?what))

        (rule (pets candace ?bird)
          (flies ?bird))

        (rule (likes sally ?who)
          (likes ?who cats)
          (drinks ?who beer))

        (facts (likes tom cats)
               (likes alice cats)
               (likes kim cats))

        (rule (likes kim ?who)
          (likes ?who cats))

        (rule (narcissist ?person)
          (likes ?person ?person)))

      )
    db))

(defparameter *test-database* (make-test-database))


;;;; Utils
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


;;;; Tests
(test facts-literal
  (with-database *test-database*
    (should-return
      ((always) empty)
      ((fuzzy cats) empty)
      ((fuzzy snakes) fail))))

(test facts-variables
  (with-database *test-database*
    (should-return
      ((fuzzy ?what)
       (?what cats))

      ((listens bob ?what)
       (?what blues)
       (?what rock))

      ((listens ?who blues)
       (?who alice)
       (?who bob)
       (?who candace))

      ((listens ?who metal) fail))))

(test facts-conjunctions
  (with-database *test-database*
    (is (results= '((?who alice))
                  (query-all (listens ?who blues)
                             (listens ?who jazz))))
    (is (results= '((?who alice))
                  (query-all (listens ?who blues)
                             (drinks ?who bourbon))))
    (is (results= '((?what bourbon ?who alice)
                    (?what genny-cream ?who bob)
                    (?what birch-beer ?who candace))
                  (query-all (listens ?who blues)
                             (drinks ?who ?what))))))

(test simple-unification
  (with-fresh-database
    (push-logic-frame-with
      (rule (= ?x ?x)))
    (should-return
      ((= x x) empty)
      ((= x y) fail)
      ((= ?x foo) (?x foo))
      ((= foo ?x) (?x foo))
      ((= (f (g foo)) ?x) (?x (f (g foo))))
      ((= (f (g foo)) (f ?x)) (?x (g foo)))
      ((= (f ?x cats) (f dogs ?y)) (?x dogs ?y cats))
      ((= (f ?x ?x) (f dogs ?y)) (?x dogs ?y dogs)))))

(test dynamic-call
  (with-fresh-database
    (push-logic-frame-with
      (facts (g cats)
             (g (f dogs)))
      (rule (normal ?x)
        (g ?x))
      (rule (dynamic ?struct)
        (call ?struct)))
    (should-return
      ((normal foo) fail)
      ((normal cats) empty)
      ((g cats) empty)
      ((call (g cats)) empty)
      ((call (g (f cats))) fail)
      ((call (nothing)) fail)
      ((call (g ?x))
       (?x cats)
       (?x (f dogs)))
      ((dynamic (g cats)) empty)
      ((dynamic (g dogs)) fail)
      ((dynamic (g (f dogs))) empty)
      ((dynamic (g ?x))
       (?x cats)
       (?x (f dogs))))))

(test not
  (with-fresh-database
    (push-logic-frame-with
      (fact (yes ?anything))

      (rule (not ?x) (call ?x) ! fail)
      (rule (not ?x)))
    (should-return
      ((yes x) empty)
      ((no x) fail)
      ((not (yes x)) fail)
      ((not (no x)) empty))))

(test backtracking
  (with-fresh-database
    (push-logic-frame-with
      (facts (b))
      (facts (c))
      (facts (d))
      (rule (f ?x) (a))
      (rule (f ?x) (b) (c))
      (rule (f ?x) (d)))
    (should-return
      ((f foo) empty)))
  (with-fresh-database
    (push-logic-frame-with
      ; (facts (a))
      (facts (b))
      (facts (c))
      (facts (d))
      (rule (f ?x) (a))
      (rule (f ?x) (b) (c))
      (rule (f ?x) (d)))
    (should-return
      ((f foo) empty)))
  (with-fresh-database
    (push-logic-frame-with
      ; (facts (a))
      (facts (b))
      (facts (c))
      ; (facts (d))
      (rule (f ?x) (a))
      (rule (f ?x) (b) (c))
      (rule (f ?x) (d)))
    (should-return
      ((f foo) empty)))
  (with-fresh-database
    (push-logic-frame-with
      ; (facts (a))
      ; (facts (b))
      (facts (c))
      ; (facts (d))
      (rule (f ?x) (a))
      (rule (f ?x) (b) (c))
      (rule (f ?x) (d)))
    (should-return
      ((f foo) fail)))
  (with-fresh-database
    (push-logic-frame-with
      ; (facts (a))
      (facts (b))
      ; (facts (c))
      ; (facts (d))
      (rule (f ?x) (a))
      (rule (f ?x) (b) (c))
      (rule (f ?x) (d)))
    (should-return
      ((f foo) fail))))

(test basic-rules
  (with-database *test-database*
    (should-fail
      (pets candace ?what))

    (should-return
      ((pets alice ?what)
       (?what snakes)
       (?what cats))

      ((pets bob ?what)
       (?what cats))

      ((pets ?who snakes)
       (?who alice))

      ((likes kim ?who)
       (?who tom)
       (?who alice)
       (?who kim)
       (?who cats))

      ((likes sally ?who)
       (?who tom))

      ((narcissist ?person)
       (?person kim)))))

(test register-allocation
  ;; test for tricky register allocation bullshit
  (with-fresh-database
    (push-logic-frame-with
      (fact (a fact-a fact-a))
      (fact (b fact-b fact-b))
      (fact (c fact-c fact-c))

      (rule (foo ?x)
        (a ?a ?a)
        (b ?b ?b)
        (c ?c ?c)))

    (should-return
      ((foo dogs) empty))))

(test lists
  (with-fresh-database
    (push-logic-frame-with
      (rule (member ?x (list* ?x ?)))
      (rule (member ?x (list* ?y ?rest))
        (member ?x ?rest)))

    (should-fail
      (member ?anything nil)
      (member a nil)
      (member b (list a))
      (member (list a) (list a))
      (member a (list (list a))))
    (should-return
      ((member ?m (list a))
       (?m a))
      ((member ?m (list a b))
       (?m a)
       (?m b))
      ((member ?m (list a b a))
       (?m a)
       (?m b))
      ((member a (list a))
       empty)
      ((member (list foo) (list a (list foo) b))
       empty))
    ;; Check that we can unify against unbound vars that turn into lists
    (is ((lambda (result)
           (eql (car (getf result '?anything)) 'a))
         (query (member a ?anything))))))

(test cut
  (with-fresh-database
    (push-logic-frame-with
      (facts (a))
      (facts (b))
      (facts (c))
      (facts (d))

      (rule (f a) (a))
      (rule (f bc) (b) ! (c))
      (rule (f d) (d))

      (rule (g ?what) (never))
      (rule (g ?what) (f ?what)))
    (should-return
      ((f ?what)
       (?what a)
       (?what bc))
      ((g ?what)
       (?what a)
       (?what bc))))

  (with-fresh-database
    (push-logic-frame-with
      ; (facts (a))
      (facts (b))
      (facts (c))
      (facts (d))

      (rule (f a) (a))
      (rule (f bc) (b) ! (c))
      (rule (f d) (d))

      (rule (g ?what) (never))
      (rule (g ?what) (f ?what)))
    (should-return
      ((f ?what)
       (?what bc))
      ((g ?what)
       (?what bc))))

  (with-fresh-database
    (push-logic-frame-with
      ; (facts (a))
      ; (facts (b))
      (facts (c))
      (facts (d))

      (rule (f a) (a))
      (rule (f bc) (b) ! (c))
      (rule (f d) (d))

      (rule (g ?what) (never))
      (rule (g ?what) (f ?what)))
    (should-return
      ((f ?what)
       (?what d))
      ((g ?what)
       (?what d))))

  (with-fresh-database
    (push-logic-frame-with
      ; (facts (a))
      (facts (b))
      ; (facts (c))
      (facts (d))

      (rule (f a) (a))
      (rule (f bc) (b) ! (c))
      (rule (f d) (d))

      (rule (g ?what) (never))
      (rule (g ?what) (f ?what)))
    (should-fail
      (f ?what)
      (g ?what)))

  (with-fresh-database
    (push-logic-frame-with
      ; (facts (a))
      ; (facts (b))
      (facts (c))
      ; (facts (d))

      (rule (f a) (a))
      (rule (f bc) (b) ! (c))
      (rule (f d) (d))

      (rule (g ?what) (never))
      (rule (g ?what) (f ?what)))
    (should-fail
      (f ?what)
      (g ?what))))

(test anonymous-variables
  (with-fresh-database
    (push-logic-frame-with
      (fact (foo x))
      (rule (bar (baz ?x ?y ?z ?thing))
        (foo ?thing))
      (fact (wild ? ? ?)))
    (should-return
      ((bar (baz a b c no)) fail)
      ((bar (baz a b c ?what)) (?what x))
      ((wild a b c) empty))))

(test normalization-ui
  (with-fresh-database
    (push-logic-frame-with
      (fact a)
      (facts (b)
             c)
      (rule dogs
        a b (c)))
    (should-return
      (a empty)
      (b empty)
      (c empty)
      (d fail)
      (dogs empty)
      ((a) empty)
      ((b) empty)
      ((c) empty)
      ((d) fail)
      (dogs empty))))
