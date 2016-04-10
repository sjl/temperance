(defpackage #:bones
  (:use #:cl)
  (:export #:hello))

(defpackage #:bones.more-utils
  (:use #:cl #:defstar #:bones.utils)
  (:export
    #:vector-push-extend-all
    #:push-if-new))

(defpackage #:bones.wam
  (:use #:cl #:defstar #:optima #:cl-arrows
        #:bones.utils #:bones.more-utils)
  (:import-from #:optima #:match)
  (:shadowing-import-from #:cl-arrows #:->))

(defpackage #:bones.paip
  (:use #:cl #:defstar #:bones.utils)
  (:documentation "Test?")
  (:export

   ;; Unification, constants
   #:unify
   #:fail #:no-bindings
   #:*check-occurs*

   ;; Destructive unification
   #:unify!
   #:unbound
   #:bound-p

   ;; Database management
   #:clear-db
   #:clear-predicate
   #:fact
   #:rule
   #:add-fact
   #:rule-fact

   ;; Lisp data structures as results
   #:return-one
   #:return-all

   ;; Interactive queries
   #:query
   #:query-one
   #:query-all
   ))

