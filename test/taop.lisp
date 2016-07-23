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
      (facts (resistor power n1)
             (resistor power n2)
             (transistor n2 ground n1)
             (transistor n3 n4 n2)
             (transistor n5 ground n4))

      (rule (inverter ?input ?output)
        (transistor ?input ground ?output)
        (resistor power ?output))

      (rule (nand ?a ?b ?output)
        (transistor ?a ?x ?output)
        (transistor ?b ground ?x)
        (resistor power ?output))

      (rule (and ?a ?b ?output)
        (nand ?a ?b ?x)
        (inverter ?x ?output)))
    (should-return
      ((and ?a ?b ?out)
       (?a n3 ?b n5 ?out n1)))

    (pop-logic-frame)

    (push-logic-frame-with
      ;; nor gate
      (facts (resistor power o)
             (transistor i1 ground o)
             (transistor i2 ground o)
             (resistor power no)
             (transistor o ground no))

      (rule (inverter ?input ?output)
        (transistor ?input ground ?output)
        (resistor power ?output))

      (rule (or ?a ?b ?output)
        (transistor ?a ground ?output)
        (transistor ?b ground ?output)
        (not (= ?a ?b))
        (resistor power ?output))

      (rule (nor ?a ?b ?output)
        (or ?a ?b ?x)
        (inverter ?x ?output)))

    (should-return
      ((or ?a ?b ?out)
       (?a i1 ?b i2 ?out o)
       (?a i2 ?b i1 ?out o))
      ((nor ?a ?b ?out)
       (?a i1 ?b i2 ?out no)
       (?a i2 ?b i1 ?out no)))

    )
  )
