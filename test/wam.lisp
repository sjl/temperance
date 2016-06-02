(in-package #:bones-test.wam)

(def-suite :bones.wam)
(in-suite :bones.wam)


;;;; Setup
(defun make-test-database ()
  (let ((db (make-database)))
    (with-database db

      (facts (always))

      (facts (drinks tom :anything)
             (drinks kim water)
             (drinks alice bourbon)
             (drinks bob genny-cream)
             (drinks candace birch-beer))

      (facts (listens alice blues)
             (listens alice jazz)
             (listens bob blues)
             (listens bob rock)
             (listens candace blues))

      (facts (fuzzy cats))

      (facts (cute cats)
             (cute snakes))

      (rules ((pets alice :what)
              (cute :what))

             ((pets bob :what)
              (cute :what)
              (fuzzy :what))

             ((pets candace :bird)
              (flies :bird)))

      (rules ((likes sally :who)
              (likes :who cats)
              (drinks :who beer))

             ((likes tom cats))
             ((likes alice cats))
             ((likes kim cats))

             ((likes kim :who)
              (likes :who cats)))

      (rules ((narcissist :person)
              (likes :person :person)))

      (rules ((member :x '(:x . :rest)))
             ((member :x '(:y . :rest))
              (member :x :rest))))
    db))

(defparameter *test-database* (make-test-database))


;;;; Utils
(defun result= (x y)
  (set-equal (plist-alist x)
             (plist-alist y)
             :test #'equal))

(defun results= (r1 r2)
  (set-equal r1 r2 :test #'result=))

(defmacro q (&body query)
  `(with-database *test-database*
    (return-all ,@query)))


(defmacro should-fail (&body queries)
  `(progn
     ,@(loop :for query :in queries :collect
             `(is (results= nil (q ,query))))))

(defmacro should-return (&body queries)
  `(progn
     ,@(loop :for (query results) :in queries :collect
             `(is (results= ',results (q ,query))))))


;;;; Tests
(test facts-literal
  (is (results= '(nil) (q (always))))
  (is (results= '(nil) (q (fuzzy cats))))
  (is (results= nil (q (fuzzy snakes)))))

(test facts-variables
  (is (results= '((:what cats))
                (q (fuzzy :what))))
  (is (results= '((:what blues)
                  (:what rock))
                (q (listens bob :what))))
  (is (results= '((:who alice)
                  (:who bob)
                  (:who candace))
                (q (listens :who blues))))
  (is (results= '()
                (q (listens :who metal)))))

(test facts-conjunctions
  (is (results= '((:who alice))
                (q (listens :who blues)
                   (listens :who jazz))))
  (is (results= '((:who alice))
                (q (listens :who blues)
                   (drinks :who bourbon))))
  (is (results= '((:what bourbon :who alice)
                  (:what genny-cream :who bob)
                  (:what birch-beer :who candace))
                (q (listens :who blues)
                   (drinks :who :what)))))

(test basic-rules
  (should-fail
    (pets candace :what))

  (should-return
    ((pets alice :what)
     ((:what snakes) (:what cats)))

    ((pets bob :what)
     ((:what cats)))

    ((pets :who snakes)
     ((:who alice)))

    ((likes kim :who)
     ((:who tom)
      (:who alice)
      (:who kim)
      (:who cats)))

    ((likes sally :who)
     ((:who tom)))

    ((narcissist :person)
     ((:person kim)))))

(test lists
  (should-fail
    (member :anything nil)
    (member a nil)
    (member b '(a))
    (member '(a) '(a))
    (member a '('(a))))
  (should-return
    ((member :m '(a))
     ((:m a)))
    ((member :m '(a b))
     ((:m a) (:m b)))
    ((member :m '(a b a))
     ((:m a) (:m b)))
    ((member a '(a))
     (nil))
    ((member '(foo) '(a '(foo) b))
     (nil))))
