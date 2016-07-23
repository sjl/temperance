(in-package #:bones-test.99)

;;;; 99 Prolog Problems
;;; http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
;;;
;;; Solutions to at least a few of these, for testing purposes.


(define-test p1
  ;; Find the last element of a list.
  (with-fresh-database
    (push-logic-frame-with
      (fact (last ?x (list ?x)))
      (rule (last ?x (list* ? ?tail))
        (last ?x ?tail)))

    (should-return
      ((last a (list a b c)) fail)
      ((last b (list a b c)) fail)
      ((last c (list a b c)) empty)

      ((last ?what nil) fail)
      ((last ?what (list a))
       (?what a))

      ((last ?what (list (h x) (f (g x))))
       (?what (f (g x))))

      ((last ?what (list (list foo)))
       (?what (foo))))))


(define-test p2
  ;; Find the last but one element of a list.
  (with-fresh-database
    (push-logic-frame-with
      (fact (last-but-one ?x (list ?x ?)))
      (rule (last-but-one ?x (list* ? ?tail))
        (last-but-one ?x ?tail)))

    (should-return
      ((last-but-one a (list a b c)) fail)
      ((last-but-one b (list a b c)) empty)
      ((last-but-one c (list a b c)) fail)

      ((last-but-one ?what nil) fail)
      ((last-but-one c (list a b c d)) empty)

      ((last-but-one ?what (list (h x) (f (g x))))
       (?what (h x)))

      ((last-but-one ?what (list (list foo) (list bar)))
       (?what (foo))))))


;;; TODO: p3/p4 when we have maths


(defun %reverse ()
  (push-logic-frame-with
    (fact (reverse-acc nil ?acc ?acc))
    (rule (reverse-acc (list* ?x ?tail) ?acc ?reversed)
      (reverse-acc ?tail (list* ?x ?acc) ?reversed))

    (rule (reverse ?l ?r)
      (reverse-acc ?l nil ?r))))

(define-test p5
  ;; Reverse a list.
  (with-fresh-database
    (%reverse)

    (should-return
      ((reverse nil nil) empty)
      ((reverse (list 1) nil) fail)
      ((reverse (list 1) (list 1)) empty)
      ((reverse (list 1 2) (list 2 1)) empty)
      ((reverse (list (f foo) (f bar))
                (list (f bar) (f foo)))
       empty)
      ((reverse (list ?x 2 3 4)
                (list ?y 3 2 1))
       (?x 1 ?y 4)))))


(define-test p6
  ;; Find out whether a list is a palindrome.
  (with-fresh-database
    (%reverse)
    (push-logic-frame-with
      (rule (palindrome ?l)
        (reverse ?l ?l)))

    (should-return
      ((palindrome nil) empty)
      ((palindrome (list 1)) empty)
      ((palindrome (list 1 1)) empty)
      ((palindrome (list 1 2)) fail)
      ((palindrome (list 1 2 1)) empty)
      ((palindrome (list (f foo) ?what))
       (?what (f foo))))))


(define-test p7
  ;; Flatten a nested list structure.
  (with-fresh-database
    (%not)
    (%append)

    (push-logic-frame-with
      (fact (is-list nil))
      (fact (is-list (list* ? ?)))

      (fact (flatten nil nil))

      (rule (flatten (list* ?atom ?tail)
                     (list* ?atom ?flat-tail))
        (not (is-list ?atom))
        (flatten ?tail ?flat-tail))

      (rule (flatten (list* ?head ?tail) ?flattened)
        (is-list ?head)
        (flatten ?head ?flat-head)
        (flatten ?tail ?flat-tail)
        (append ?flat-head ?flat-tail ?flattened)))

    (should-return
      ((is-list nil) empty)
      ((is-list (list a)) empty)
      ((is-list (list a b)) empty)
      ((is-list (f x)) fail)
      ((is-list a) fail)

      ((flatten nil ?what)
       (?what nil))

      ((flatten (list a) ?what)
       (?what (a)))

      ((flatten (list (list a)) ?what)
       (?what (a)))

      ((flatten (list (list a b) (list (list c))) ?what)
       (?what (a b c))))))


(define-test p8
  ;; Eliminate consecutive duplicates of list elements.
  (with-fresh-database
    (%=)
    (%not)

    (push-logic-frame-with
      (fact (compress nil nil))
      (fact (compress (list ?x) (list ?x)))

      (rule (compress (list* ?x ?x ?rest) ?result)
        (compress (list* ?x ?rest) ?result))

      (rule (compress (list* ?x ?y ?rest) (list* ?x ?result))
        (not (= ?x ?y))
        (compress (list* ?y ?rest) ?result)))

    (should-return
      ((compress nil ?what)
       (?what nil))

      ((compress (list a) ?what)
       (?what (a)))

      ((compress (list a b c) ?what)
       (?what (a b c)))

      ((compress (list a b b a) ?what)
       (?what (a b a)))

      ((compress (list (f cats ?) ?what (f ? dogs))
                 (list ?))
       (?what (f cats dogs))))))

; (define-test p9
;   (with-fresh-database
;     (%=)
;     (%not)

;     (push-logic-frame-with
;       (fact (pack nil nil))
;       (fact (pack (list ?x) (list (list ?x))))

;       (rule (pack (list* ?x ?tail)
;                   (list* (list ?x) ?ptail))
;         (pack ?tail ?ptail)
;         (= ?ptail (list* (list* ?y ?) ?))
;         (not (= ?x ?y)))

;       (rule (pack (list* ?h ?tail)
;                   (list* (list* ?h ?h ?hs) ?more))
;         (pack ?tail (list* (list* ?h ?hs) ?more))))

;     (should-return
;       ((pack nil nil) empty)
;       ((pack (list a) ?what)
;        (?what ((a))))
;       ((pack (list a a) ?what)
;        (?what ((a a))))
;       ((pack (list a a a) ?what)
;        (?what ((a a a))))
;       ((pack (list a a b a) ?what)
;        (?what ((a a) (b) (a)))))))
