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
              (likes :person :person))))
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

(defmacro check (query)
  `(with-database *test-database*
    (nth-value 1 (return-one ,query))))


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
  (is (results= '((:what snakes)
                  (:what cats))
                (q (pets alice :what))))

  (is (results= '((:what cats))
                (q (pets bob :what))))

  (is (results= '()
                (q (pets candace :what))))

  (is (results= '((:who alice))
                (q (pets :who snakes))))

  (is (results= '((:who tom)
                  (:who alice)
                  (:who kim)
                  (:who cats))
                (q (likes kim :who))))

  (is (results= '((:who tom))
                (q (likes sally :who))))

  (is (results= '((:person kim))
                (q (narcissist :person)))))
