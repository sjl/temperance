(in-package #:bones-test.taop)

;; Examples and exercises from The Art of Prolog

(define-test families
  (with-fresh-database
    (%=)
    (%not)
    (push-logic-frame-with
      (facts (father abraham isaac)
             (father haran lot)
             (father haran milcah)
             (father haran yiscah)
             (father terach abraham)
             (father terach nachor)
             (father terach haran)
             (mother sarah isaac)
             (male isaac)
             (male lot)
             (male terach)
             (male nachor)
             (male haran)
             (male abraham)
             (female sarah)
             (female milcah)
             (female yiscah))

      (rule (parent ?person ?kid) (father ?person ?kid))
      (rule (parent ?person ?kid) (mother ?person ?kid))

      (rule (father ?person) (father ?person ?))
      (rule (mother ?person) (mother ?person ?))
      (rule (parent ?person) (father ?person))
      (rule (parent ?person) (mother ?person))

      (rule (grandparent ?person ?grandkid)
        (parent ?person ?kid)
        (parent ?kid ?grandkid))

      (rule (grandmother ?person ?grandkid)
        (grandparent ?person ?grandkid)
        (female ?person))

      (rule (grandfather ?person ?grandkid)
        (grandparent ?person ?grandkid)
        (male ?person))

      (rule (son ?parent ?kid)
        (parent ?parent ?kid)
        (male ?kid))

      (rule (daughter ?parent ?kid)
        (parent ?parent ?kid)
        (female ?kid))

      (rule (siblings ?x ?y)
        (parent ?p ?x)
        (parent ?p ?y)
        (not (= ?x ?y)))

      (rule (brother ?bro ?person)
        (siblings ?bro ?person)
        (male ?bro))

      (rule (sister ?sis ?person)
        (siblings ?sis ?person)
        (female ?sis))

      (rule (uncle ?unc ?kid)
        (brother ?unc ?parent)
        (parent ?parent ?kid))

      (rule (aunt ?unc ?kid)
        (sister ?unc ?parent)
        (parent ?parent ?kid))

      (rule (cousins ?x ?y)
        (parent ?px ?x)
        (parent ?py ?y)
        (siblings ?px ?py)))

    (should-return
      ((father ?who)
       (?who abraham)
       (?who haran)
       (?who terach))
      ((grandfather ?who ?kid)
       (?who terach ?kid isaac)
       (?who terach ?kid lot)
       (?who terach ?kid milcah)
       (?who terach ?kid yiscah))
      ((brother ?who abraham)
       (?who nachor)
       (?who haran))
      ((cousins isaac ?who)
       (?who lot)
       (?who milcah)
       (?who yiscah))
      ((uncle ?who lot)
       (?who abraham)
       (?who nachor))
      ((uncle ?who isaac)
       (?who haran)
       (?who nachor)))))

(define-test circuits
  (with-fresh-database
    (%=)
    (%not)

    (push-logic-frame-with
      ;; and gate

      ;; (resistor name node-1 node-2)
      (facts (resistor r1 power n1)
             (resistor r2 power n2))

      ;; (transistor name gate source drain)
      (facts (transistor t1 n2 ground n1)
             (transistor t2 n3 n4 n2)
             (transistor t3 n5 ground n4))

      (rule (inverter (inverter ?t ?r) ?input ?output)
        (transistor ?t ?input ground ?output)
        (resistor ?r power ?output))

      (rule (nand (nand ?t1 ?t2 ?r) ?a ?b ?output)
        (transistor ?t1 ?a ?x ?output)
        (transistor ?t2 ?b ground ?x)
        (resistor ?r power ?output))

      (rule (and (and ?n ?i) ?a ?b ?output)
        (nand ?n ?a ?b ?x)
        (inverter ?i ?x ?output)))

    (should-return
      ((and ?g ?a ?b ?out)
       (?a n3 ?b n5 ?out n1 ?g (and (nand t2 t3 r2)
                                    (inverter t1 r1)))))

    (pop-logic-frame)

    (push-logic-frame-with
      ;; nor gate
      (facts (resistor r1 power o)
             (transistor t1 i1 ground o)
             (transistor t2 i2 ground o)
             (resistor r2 power no)
             (transistor t3 o ground no))

      (rule (inverter (inverter ?t ?r) ?input ?output)
        (transistor ?t ?input ground ?output)
        (resistor ?r power ?output))

      (rule (or (or ?t1 ?t2 ?r) ?a ?b ?output)
        (transistor ?t1 ?a ground ?output)
        (transistor ?t2 ?b ground ?output)
        (not (= ?a ?b))
        (resistor ?r power ?output))

      (rule (nor (nor ?o ?i) ?a ?b ?output)
        (or ?o ?a ?b ?x)
        (inverter ?i ?x ?output)))

    (should-return
      ((or ?g ?a ?b ?out)
       (?a i1 ?b i2 ?out o ?g (or t1 t2 r1))
       (?a i2 ?b i1 ?out o ?g (or t2 t1 r1)))
      ((nor ?g ?a ?b ?out)
       (?a i1 ?b i2 ?out no ?g (nor (or t1 t2 r1)
                                    (inverter t3 r2)))
       (?a i2 ?b i1 ?out no ?g (nor (or t2 t1 r1)
                                    (inverter t3 r2)))))))

(define-test courses
  (with-fresh-database
    (push-logic-frame-with
      (facts (course complexity
                     (time monday 9 11)
                     (lecturer david harel)
                     (location feinberg a))
             (course lisp
                     (time monday 10 12)
                     (lecturer alyssa p hacker)
                     (location main-hall))
             (course scheme
                     (time monday 12 15)
                     (lecturer alyssa p hacker)
                     (location online))
             (course prolog
                     (time tuesday 12 15)
                     (lecturer ben bitdiddle)
                     (location feinberg b))
             (course haskell
                     (time wednesday 12 15)
                     (lecturer ben bitdiddle)
                     (location online)))

      (rule (lecturer ?who ?course)
        (course ?course ? ?who ?))

      (rule (teaches-on ?who ?day)
        (course ? (time ?day ? ?) ?who ?))

      (rule (teaches-in ?who ?location)
        (course ? ? ?who ?location))

      (rule (location-of ?course ?location)
        (course ?course ? ? ?location)))

    (should-return
      ((lecturer ?who lisp)
       (?who (lecturer alyssa p hacker)))

      ((teaches-in (lecturer alyssa p hacker) ?loc)
       (?loc (location online))
       (?loc (location main-hall)))

      ((location-of lisp ?where)
       (?where (location main-hall)))

      ((teaches-on ?who ?day)
       (?who (lecturer alyssa p hacker) ?day monday)
       (?who (lecturer ben bitdiddle) ?day tuesday)
       (?who (lecturer ben bitdiddle) ?day wednesday)
       (?who (lecturer david harel) ?day monday)))))


(define-test books
  (with-fresh-database
    (%member)
    (push-logic-frame-with
      (facts (book paip (list norvig) 1992)
             (book sicp (list abelson sussman) 1996)
             (book lol (list hoyte) 2008)
             (book clos (list keene) 1988))

      (rule (wrote ?who ?title)
        (book ?title ?authors ?)
        (member ?who ?authors))

      (rule (published-in ?who ?year)
        (book ? ?authors ?year)
        (member ?who ?authors)))

    (should-return
      ((wrote sussman ?what)
       (?what sicp))
      ((published-in keene ?year)
       (?year 1988))
      ((published-in ?who 1996)
       (?who abelson)
       (?who sussman)))))
