(in-package :temperance-test.taop)

;; Examples and exercises from The Art of Prolog

(define-test families
  (with-fresh-database
    (%=)
    (%not)
    (push-logic-frame-with t
      (facts t
        (father abraham isaac)
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

      (rule t (parent ?person ?kid) (father ?person ?kid))
      (rule t (parent ?person ?kid) (mother ?person ?kid))

      (rule t (father ?person) (father ?person ?))
      (rule t (mother ?person) (mother ?person ?))
      (rule t (parent ?person) (father ?person))
      (rule t (parent ?person) (mother ?person))

      (rule t (grandparent ?person ?grandkid)
        (parent ?person ?kid)
        (parent ?kid ?grandkid))

      (rule t (grandmother ?person ?grandkid)
        (grandparent ?person ?grandkid)
        (female ?person))

      (rule t (grandfather ?person ?grandkid)
        (grandparent ?person ?grandkid)
        (male ?person))

      (rule t (son ?parent ?kid)
        (parent ?parent ?kid)
        (male ?kid))

      (rule t (daughter ?parent ?kid)
        (parent ?parent ?kid)
        (female ?kid))

      (rule t (siblings ?x ?y)
        (parent ?p ?x)
        (parent ?p ?y)
        (not (= ?x ?y)))

      (rule t (brother ?bro ?person)
        (siblings ?bro ?person)
        (male ?bro))

      (rule t (sister ?sis ?person)
        (siblings ?sis ?person)
        (female ?sis))

      (rule t (uncle ?unc ?kid)
        (brother ?unc ?parent)
        (parent ?parent ?kid))

      (rule t (aunt ?unc ?kid)
        (sister ?unc ?parent)
        (parent ?parent ?kid))

      (rule t (cousins ?x ?y)
        (parent ?px ?x)
        (parent ?py ?y)
        (siblings ?px ?py))

      (rule t (ancestor ?old ?young)
        (parent ?old ?young))

      (rule t (ancestor ?old ?young)
        (parent ?old ?p)
        (ancestor ?p ?young)))

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
       (?who nachor))
      ((ancestor ?who isaac)
       (?who abraham)
       (?who sarah)
       (?who terach)))))

(define-test circuits
  (with-fresh-database
    (%=)
    (%not)

    (push-logic-frame-with t
      ;; and gate

      ;; (resistor name node-1 node-2)
      (facts t
        (resistor r1 power n1)
        (resistor r2 power n2))

      ;; (transistor name gate source drain)
      (facts t
        (transistor t1 n2 ground n1)
        (transistor t2 n3 n4 n2)
        (transistor t3 n5 ground n4))

      (rule t (inverter (inverter ?t ?r) ?input ?output)
        (transistor ?t ?input ground ?output)
        (resistor ?r power ?output))

      (rule t (nand (nand ?t1 ?t2 ?r) ?a ?b ?output)
        (transistor ?t1 ?a ?x ?output)
        (transistor ?t2 ?b ground ?x)
        (resistor ?r power ?output))

      (rule t (and (and ?n ?i) ?a ?b ?output)
        (nand ?n ?a ?b ?x)
        (inverter ?i ?x ?output)))

    (should-return
      ((and ?g ?a ?b ?out)
       (?a n3 ?b n5 ?out n1 ?g (and (nand t2 t3 r2)
                                    (inverter t1 r1)))))

    (pop-logic-frame t)

    (push-logic-frame-with t
      ;; nor gate
      (facts t
        (resistor r1 power o)
        (transistor t1 i1 ground o)
        (transistor t2 i2 ground o)
        (resistor r2 power no)
        (transistor t3 o ground no))

      (rule t (inverter (inverter ?t ?r) ?input ?output)
        (transistor ?t ?input ground ?output)
        (resistor ?r power ?output))

      (rule t (or (or ?t1 ?t2 ?r) ?a ?b ?output)
        (transistor ?t1 ?a ground ?output)
        (transistor ?t2 ?b ground ?output)
        (not (= ?a ?b))
        (resistor ?r power ?output))

      (rule t (nor (nor ?o ?i) ?a ?b ?output)
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
    (push-logic-frame-with t
      (facts t
        (course complexity
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

      (rule t (lecturer ?who ?course)
        (course ?course ? ?who ?))

      (rule t (teaches-on ?who ?day)
        (course ? (time ?day ? ?) ?who ?))

      (rule t (teaches-in ?who ?location)
        (course ? ? ?who ?location))

      (rule t (location-of ?course ?location)
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
    (push-logic-frame-with t
      (facts t
        (book paip (list norvig) 1992)
        (book sicp (list abelson sussman) 1996)
        (book lol (list hoyte) 2008)
        (book clos (list keene) 1988))

      (rule t (wrote ?who ?title)
        (book ?title ?authors ?)
        (member ?who ?authors))

      (rule t (published-in ?who ?year)
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

(define-test graph
  (with-fresh-database
    (push-logic-frame-with t
      (facts t
        (edge a b)
        (edge c d)
        (edge a c)
        (edge d e)
        (edge b d)
        (edge f g))

      (fact t (connected ?node ?node))
      (rule t (connected ?node-1 ?node-2)
        (edge ?node-1 ?link)
        (connected ?link ?node-2)))

    (should-return
      ((connected a a) empty)
      ((connected a e) empty)
      ((connected f g) empty)
      ((connected e f) fail)
      ((connected g f) fail))))


(define-test relational-databases
  (with-fresh-database
    (%not)
    (push-logic-frame-with t
      (facts t
        (r a b 1)
        (r a a 2)
        (r b b 0)

        (s a b 1)
        (s a a 0)
        (s b b 2))

      ;; union
      (rule t (r-union-s ?x1 ?x2 ?x3) (r ?x1 ?x2 ?x3))
      (rule t (r-union-s ?x1 ?x2 ?x3) (s ?x1 ?x2 ?x3))

      ;; difference
      (rule t (r-diff-s ?x1 ?x2 ?x3)
        (r ?x1 ?x2 ?x3)
        (not (s ?x1 ?x2 ?x3)))

      (rule t (r-diff-s ?x1 ?x2 ?x3)
        (s ?x1 ?x2 ?x3)
        (not (r ?x1 ?x2 ?x3)))

      ;; cartesian product
      (rule t (r-cart-s ?x1 ?x2 ?x3 ?x4 ?x5 ?x6)
        (r ?x1 ?x2 ?x3)
        (s ?x4 ?x5 ?x6))

      ;; projection
      (rule t (r-proj-13 ?x1 ?x3)
        (r ?x1 ? ?x3))

      ;; selection
      (facts t
        (odd 1)
        (even 0)
        (even 2))

      (rule t (r-sel-odd ?x1 ?x2 ?x3)
        (r ?x1 ?x2 ?x3)
        (odd ?x3))

      (rule t (s-sel-even ?x1 ?x2 ?x3)
        (s ?x1 ?x2 ?x3)
        (even ?x3))

      ;; intersection
      (rule t (r-intersect-s ?x1 ?x2 ?x3)
        (r ?x1 ?x2 ?x3)
        (s ?x1 ?x2 ?x3))

      ;; natural join
      (rule t (r-join-s ?r1 ?r2 ?s1 ?s2 ?n)
        (r ?r1 ?r2 ?n)
        (s ?s1 ?s2 ?n)))

    (should-return
      ((r-union-s ?x ?y ?z)
       (?x a ?y b ?z 1)
       (?x a ?y a ?z 2)
       (?x b ?y b ?z 0)
       (?x a ?y a ?z 0)
       (?x b ?y b ?z 2))

      ((r-diff-s ?x ?y ?z)
       (?x a ?y a ?z 2)
       (?x b ?y b ?z 0)
       (?x a ?y a ?z 0)
       (?x b ?y b ?z 2))

      ((r-cart-s ?p ?q ?r ?x ?y ?z)
       (?p a ?q b ?r 1 ?x a ?y b ?z 1)
       (?p a ?q b ?r 1 ?x a ?y a ?z 0)
       (?p a ?q b ?r 1 ?x b ?y b ?z 2)

       (?p a ?q a ?r 2 ?x a ?y b ?z 1)
       (?p a ?q a ?r 2 ?x a ?y a ?z 0)
       (?p a ?q a ?r 2 ?x b ?y b ?z 2)

       (?p b ?q b ?r 0 ?x a ?y b ?z 1)
       (?p b ?q b ?r 0 ?x a ?y a ?z 0)
       (?p b ?q b ?r 0 ?x b ?y b ?z 2))

      ((r-proj-13 ?x ?y)
       (?x a ?y 1)
       (?x a ?y 2)
       (?x b ?y 0))

      ((r-sel-odd ?x ?y ?z)
       (?x a ?y b ?z 1))
      ((s-sel-even ?x ?y ?z)
       (?x a ?y a ?z 0)
       (?x b ?y b ?z 2))

      ((r-intersect-s ?x ?y ?z)
       (?x a ?y b ?z 1))

      ((r-join-s ?r1 ?r2 ?s1 ?s2 ?n)
       (?r1 b ?r2 b ?s1 a ?s2 a ?n 0)
       (?r1 a ?r2 b ?s1 a ?s2 b ?n 1)
       (?r1 a ?r2 a ?s1 b ?s2 b ?n 2)))))
