(defpackage #:bones.utils
  (:use
    #:cl
    #:defstar
    #:cl-arrows
    #:bones.quickutils)
  (:export
    #:yolo
    #:repeat
    #:hex
    #:push-if-new
    #:array-push
    #:recursively
    #:recur
    #:when-let
    #:symbolize
    #:dis
    #:megabytes
    #:ecase/tree
    #:gethash-or-init
    #:define-lookup
    #:queue
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
    #:circle
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
  (:export
    #:make-database
    #:reset-database
    #:with-database
    #:with-fresh-database

    #:invoke-rule
    #:invoke-fact
    #:invoke-facts

    #:rule
    #:fact
    #:facts

    #:push-logic-frame
    #:pop-logic-frame
    #:finalize-logic-frame
    #:push-logic-frame-with

    #:invoke-query
    #:invoke-query-all
    #:invoke-query-map
    #:invoke-query-do
    #:invoke-query-find
    #:invoke-prove

    #:query
    #:query-all
    #:query-map
    #:query-do
    #:query-find
    #:prove

    #:call
    #:?
    #:!
    )
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


(defpackage #:bones
  (:use #:cl #:bones.wam)
  (:export
    #:make-database
    #:with-database
    #:with-fresh-database

    #:invoke-rule
    #:invoke-fact
    #:invoke-facts

    #:rule
    #:fact
    #:facts

    #:push-logic-frame
    #:pop-logic-frame
    #:finalize-logic-frame
    #:push-logic-frame-with

    #:invoke-query
    #:invoke-query-all
    #:invoke-query-map
    #:invoke-query-do
    #:invoke-query-find
    #:invoke-prove

    #:query
    #:query-all
    #:query-map
    #:query-do
    #:query-find
    #:prove

    #:call
    #:?
    #:!

    ))
