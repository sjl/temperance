(defpackage #:bones
  (:use #:cl)
  (:export #:hello))

(defpackage #:bones.wam
  (:use #:cl #:defstar #:bones.utils #:optima)
  (:import-from #:optima #:match))

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
   #:fact
   #:rule

   ;; Lisp data structures as results
   #:return-one
   #:return-all

   ;; Interactive queries
   #:query
   #:query-one
   #:query-all
   ))

