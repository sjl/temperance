(defpackage #:bones
  (:use #:cl)
  (:export #:hello))

(defpackage #:bones.utils
  (:use
    #:cl
    #:defstar
    #:cl-arrows
    #:bones.quickutils)
  (:export
    #:repeat
    #:hex
    #:push-if-new
    #:recursively
    #:recur
    #:when-let
    #:unique-items
    #:dis
    #:gethash-or-init
    #:make-queue
    #:enqueue
    #:dequeue
    #:queue-contents
    #:queue-empty-p
    #:queue-append)
  (:shadowing-import-from #:cl-arrows
    #:->))

(defpackage #:bones.circle
  (:use #:cl #:defstar)
  (:export
    #:make-circle-with
    #:make-empty-circle
    #:circle-to-list
    #:circle-prepend
    #:circle-prepend-circle
    #:circle-append
    #:circle-append-circle
    #:circle-next
    #:circle-prev
    #:circle-forward
    #:circle-backward
    #:circle-value
    #:circle-rotate
    #:circle-nth
    #:circle-insert-before
    #:circle-insert-after
    #:circle-sentinel-p
    #:circle-empty-p
    #:circle-remove
    #:circle-backward-remove
    #:circle-forward-remove
    #:circle-replace
    #:circle-backward-replace
    #:circle-forward-replace
    #:circle-splice
    #:circle-backward-splice
    #:circle-forward-splice
    #:circle-insert-beginning
    #:circle-insert-end))

(defpackage #:bones.wam
  (:use
    #:cl
    #:defstar
    #:optima
    #:cl-arrows
    #:bones.circle
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
    #:query-all))

