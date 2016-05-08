(defpackage #:bones
  (:use #:cl)
  (:export #:hello))

(defpackage #:bones.utils
  (:use
    #:cl
    #:defstar
    #:bones.quickutils)
  (:export
    #:repeat
    #:hex
    #:topological-sort
    #:push-if-new))

(defpackage #:bones.wam
  (:use
    #:cl
    #:defstar
    #:optima
    #:cl-arrows
    #:bones.quickutils
    #:bones.utils)
  (:import-from #:optima
    #:match)
  (:shadowing-import-from #:cl-arrows
    #:->))

(defpackage #:bones.paip
  (:use
    #:cl
    #:defstar
    #:bones.quickutils)
  (:documentation "Test?")
  (:export

    ;; Unification, constants
    #:unify
    #:fail
    #:no-bindings
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

