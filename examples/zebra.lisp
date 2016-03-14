(in-package #:bones.paip)

(clear-db)

(rule (member ?item (?item . ?)))
(rule (member ?item (? . ?rest))
      (member ?item ?rest))

(rule (next-to ?x ?y ?list)
      (in-order ?x ?y ?list))

(rule (next-to ?x ?y ?list)
      (in-order ?y ?x ?list))

(rule (in-order ?x ?y (?x ?y . ?)))
(rule (in-order ?x ?y (? . ?rest))
      (in-order ?x ?y ?rest))

(rule (= ?x ?x))

(rule
 (zebra ?houses ?water-drinker ?zebra-owner)
 ;; Houses are of the form:
 ;; (HOUSE ?country ?pet ?cigarette ?drink ?color)

 (= ?houses
    ((house norway ? ? ? ?)
     ?
     (house ? ? ? milk ?)
     ?
     ?))

 (member   (house england ?      ?            ?            red   ) ?houses)
 (member   (house spain   dog    ?            ?            ?     ) ?houses)
 (member   (house ?       ?      ?            coffee       green ) ?houses)
 (member   (house ukraine ?      ?            tea          ?     ) ?houses)
 (member   (house ?       snails winston      ?            ?     ) ?houses)
 (member   (house ?       ?      kools        ?            yellow) ?houses)
 (member   (house ?       ?      lucky-strike orange-juice ?     ) ?houses)
 (member   (house japan   ?      parliaments  ?            ?     ) ?houses)
 (in-order (house ?       ?      ?            ?            ivory )
           (house ?       ?      ?            ?            green ) ?houses)
 (next-to  (house ?       ?      chesterfield ?            ?     )
           (house ?       fox    ?            ?            ?     ) ?houses)
 (next-to  (house ?       ?      kools        ?            ?     )
           (house ?       horse  ?            ?            ?     ) ?houses)
 (next-to  (house norway  ?      ?            ?            ?     )
           (house ?       ?      ?            ?            blue  ) ?houses)

 (member (house ?water-drinker ? ? water ?) ?houses)
 (member (house ?zebra-owner zebra ? ? ?) ?houses))

(time
  (query-all (zebra ?houses ?water ?zebra)))
