(in-package :temperance.test.wam)

;;;; Setup
(defun make-test-database ()
  (let ((db (make-database)))
    (push-logic-frame-with db

      (facts db
        (always)

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

      (rule db (pets alice ?what)
        (cute ?what))

      (rule db (pets bob ?what)
        (cute ?what)
        (fuzzy ?what))

      (rule db (pets candace ?bird)
        (flies ?bird))

      (rule db (likes sally ?who)
        (likes ?who cats)
        (drinks ?who beer))

      (facts db (likes tom cats)
        (likes alice cats)
        (likes kim cats))

      (rule db (likes kim ?who)
        (likes ?who cats))

      (rule db (narcissist ?person)
        (likes ?person ?person)))

    db))

(defparameter *test-database* (make-test-database))


;;;; Tests
(define-test facts-literal
  (with-database *test-database*
    (should-return
      ((always) empty)
      ((fuzzy cats) empty)
      ((fuzzy snakes) fail))))

(define-test facts-variables
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

(define-test facts-conjunctions
  (with-database *test-database*
    (is (results= '((?who alice))
                  (query-all t
                             (listens ?who blues)
                             (listens ?who jazz))))
    (is (results= '((?who alice))
                  (query-all t
                             (listens ?who blues)
                             (drinks ?who bourbon))))
    (is (results= '((?what bourbon ?who alice)
                    (?what genny-cream ?who bob)
                    (?what birch-beer ?who candace))
                  (query-all t
                             (listens ?who blues)
                             (drinks ?who ?what))))))

(define-test simple-unification
  (with-fresh-database
    (push-logic-frame-with t
      (rule t (= ?x ?x)))
    (should-return
      ((= x x) empty)
      ((= x y) fail)
      ((= ?x foo) (?x foo))
      ((= foo ?x) (?x foo))
      ((= (f (g foo)) ?x) (?x (f (g foo))))
      ((= (f (g foo)) (f ?x)) (?x (g foo)))
      ((= (f ?x cats) (f dogs ?y)) (?x dogs ?y cats))
      ((= (f ?x ?x) (f dogs ?y)) (?x dogs ?y dogs)))))

(define-test dynamic-call
  (with-fresh-database
    (push-logic-frame-with t
      (facts t (g cats)
        (g (f dogs)))
      (rule t (normal ?x)
        (g ?x))
      (rule t (dynamic ?struct)
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

(define-test negation
  (with-fresh-database
    (push-logic-frame-with t
      (fact t (yes ?anything))

      (rule t (not ?x) (call ?x) ! fail)
      (rule t (not ?x)))
    (should-return
      ((yes x) empty)
      ((no x) fail)
      ((not (yes x)) fail)
      ((not (no x)) empty))))

(define-test backtracking
  (with-fresh-database
    (push-logic-frame-with t
      (facts t (b))
      (facts t (c))
      (facts t (d))
      (rule t (f ?x) (a))
      (rule t (f ?x) (b) (c))
      (rule t (f ?x) (d)))
    (should-return
      ((f foo) empty)))
  (with-fresh-database
    (push-logic-frame-with t
      ; (facts t (a))
      (facts t (b))
      (facts t (c))
      (facts t (d))
      (rule t (f ?x) (a))
      (rule t (f ?x) (b) (c))
      (rule t (f ?x) (d)))
    (should-return
      ((f foo) empty)))
  (with-fresh-database
    (push-logic-frame-with t
      ; (facts t (a))
      (facts t (b))
      (facts t (c))
      ; (facts t (d))
      (rule t (f ?x) (a))
      (rule t (f ?x) (b) (c))
      (rule t (f ?x) (d)))
    (should-return
      ((f foo) empty)))
  (with-fresh-database
    (push-logic-frame-with t
      ; (facts t (a))
      ; (facts t (b))
      (facts t (c))
      ; (facts t (d))
      (rule t (f ?x) (a))
      (rule t (f ?x) (b) (c))
      (rule t (f ?x) (d)))
    (should-return
      ((f foo) fail)))
  (with-fresh-database
    (push-logic-frame-with t
      ; (facts t (a))
      (facts t (b))
      ; (facts t (c))
      ; (facts t (d))
      (rule t (f ?x) (a))
      (rule t (f ?x) (b) (c))
      (rule t (f ?x) (d)))
    (should-return
      ((f foo) fail))))

(define-test basic-rules
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

(define-test register-allocation
  ;; test for tricky register allocation bullshit
  (with-fresh-database
    (push-logic-frame-with t
      (fact t (a fact-a fact-a))
      (fact t (b fact-b fact-b))
      (fact t (c fact-c fact-c))

      (rule t (foo ?x)
        (a ?a ?a)
        (b ?b ?b)
        (c ?c ?c)))

    (should-return
      ((foo dogs) empty))))

(define-test lists
  (with-fresh-database
    (push-logic-frame-with t
      (rule t (member ?x (list* ?x ?)))
      (rule t (member ?x (list* ?y ?rest))
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
         (query t (member a ?anything))))))

(define-test cut
  (with-fresh-database
    (push-logic-frame-with t
      (facts t (a))
      (facts t (b))
      (facts t (c))
      (facts t (d))

      (rule t (f a) (a))
      (rule t (f bc) (b) ! (c))
      (rule t (f d) (d))

      (rule t (g ?what) (never))
      (rule t (g ?what) (f ?what)))
    (should-return
      ((f ?what)
       (?what a)
       (?what bc))
      ((g ?what)
       (?what a)
       (?what bc))))

  (with-fresh-database
    (push-logic-frame-with t
      ; (facts t (a))
      (facts t (b))
      (facts t (c))
      (facts t (d))

      (rule t (f a) (a))
      (rule t (f bc) (b) ! (c))
      (rule t (f d) (d))

      (rule t (g ?what) (never))
      (rule t (g ?what) (f ?what)))
    (should-return
      ((f ?what)
       (?what bc))
      ((g ?what)
       (?what bc))))

  (with-fresh-database
    (push-logic-frame-with t
      ; (facts t (a))
      ; (facts t (b))
      (facts t (c))
      (facts t (d))

      (rule t (f a) (a))
      (rule t (f bc) (b) ! (c))
      (rule t (f d) (d))

      (rule t (g ?what) (never))
      (rule t (g ?what) (f ?what)))
    (should-return
      ((f ?what)
       (?what d))
      ((g ?what)
       (?what d))))

  (with-fresh-database
    (push-logic-frame-with t
      ; (facts t (a))
      (facts t (b))
      ; (facts t (c))
      (facts t (d))

      (rule t (f a) (a))
      (rule t (f bc) (b) ! (c))
      (rule t (f d) (d))

      (rule t (g ?what) (never))
      (rule t (g ?what) (f ?what)))
    (should-fail
      (f ?what)
      (g ?what)))

  (with-fresh-database
    (push-logic-frame-with t
      ; (facts t (a))
      ; (facts t (b))
      (facts t (c))
      ; (facts t (d))

      (rule t (f a) (a))
      (rule t (f bc) (b) ! (c))
      (rule t (f d) (d))

      (rule t (g ?what) (never))
      (rule t (g ?what) (f ?what)))
    (should-fail
      (f ?what)
      (g ?what))))

(define-test anonymous-variables
  (with-fresh-database
    (push-logic-frame-with t
      (fact t (following (s ? ? ? a)))
      (fact t (foo x))
      (rule t (bar (baz ?x ?y ?z ?thing))
        (foo ?thing))
      (fact t (wild ? ? ?))

      (fact t (does x move))
      (rule t (next z)
        (does ? move)))
    (should-return
      ((following (s x x x a)) empty)
      ((bar (baz a b c no)) fail)
      ((bar (baz a b c ?what)) (?what x))
      ((wild a b c) empty)
      ((next z) empty)
      )))

(define-test normalization-ui
  (with-fresh-database
    (push-logic-frame-with t
      (fact t a)
      (facts t (b)
        c)
      (rule t dogs
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

(define-test nested-constants
  (with-fresh-database
    (push-logic-frame-with t
      (fact t (foo (s a b c))))
    (should-return
      ((foo (s ?x ?y ?z))
       (?x a ?y b ?z c)))))

(define-test dump
  (is (not (string= ""
                    (with-output-to-string (*standard-output*)
                      (dump-wam-full *test-database*))))))

(define-test last-call-optimization
  (let* ((big-ass-list (loop :repeat 1000 :collect 'a))
         (big-ass-result (reverse (cons 'x big-ass-list))))
    (with-fresh-database
      (push-logic-frame-with t
        (invoke-fact t `(big-ass-list (list ,@big-ass-list)))

        (fact t (append nil ?l ?l))
        (rule t (append (list* ?i ?tail) ?other (list* ?i ?l))
          (append ?tail ?other ?l)))

      (is (results= `((?bal ,big-ass-list ?bar ,big-ass-result))
                    (query-all t
                               (big-ass-list ?bal)
                               (append ?bal (list x) ?bar)))))))

(define-test hanoi
  ;; From The Art of Prolog
  (with-fresh-database
    (push-logic-frame-with t
      (fact t (append nil ?l ?l))
      (rule t (append (list* ?i ?tail) ?other (list* ?i ?l))
        (append ?tail ?other ?l))

      (fact t (hanoi zero ?a ?b ?c nil))
      (rule t (hanoi (s ?n) ?a ?b ?c ?moves)
        (hanoi ?n ?a ?c ?b ?moves1)
        (hanoi ?n ?c ?b ?a ?moves2)
        (append ?moves1 (list* (move ?a ?b) ?moves2) ?moves)))
    (should-return
      ((hanoi zero a b c ?what)
       (?what nil))
      ((hanoi (s zero) a b c ?what)
       (?what ((move a b))))
      ((hanoi (s (s zero)) a b c ?what)
       (?what ((move a c) (move a b) (move c b))))
      ((hanoi (s (s (s zero))) a b c ?what)
       (?what ((move a b) (move a c) (move b c)
               (move a b)
               (move c a) (move c b) (move a b)))))))

(define-test numbers
  (with-fresh-database
    (push-logic-frame-with t
      (rule t (= ?x ?x))
      (fact t (foo 1))
      (fact t (bar 2))
      (rule t (baz ?x) (foo ?x))
      (rule t (baz ?x) (bar ?x))
      (rule t (lol ?x)
        (foo ?x)
        (bar ?x)))

    (should-return
      ((foo ?what)
       (?what 1))
      ((bar ?what)
       (?what 2))
      ((baz ?what)
       (?what 1)
       (?what 2))
      ((foo 0) fail)
      ((lol ?anything) fail)
      ((= 0 1) fail)
      ((= 0 0) empty))))

