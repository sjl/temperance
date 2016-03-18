(defpackage #:bones
  (:use #:cl)
  (:export #:hello))

(defpackage #:bones.paip
  (:use #:cl #:defstar #:bones.utils)
  (:documentation "Test?")
  (:export

   ;; Unification, constants
   #:unify
   #:fail #:no-bindings
   #:*check-occurs*

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

