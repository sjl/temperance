(defpackage #:bones-test
  (:use #:cl #:1am))

(defpackage #:bones-test.utils
  (:use
    #:cl
    #:1am
    #:bones.wam
    #:bones.quickutils)
  (:export
    #:fail
    #:empty
    #:result=
    #:results=
    #:should-fail
    #:should-return
    #:define-test
    #:%=
    #:%not
    #:%append))

(defpackage #:bones-test.paip
  (:use
    #:cl
    #:1am
    #:bones.quickutils
    #:bones.paip))

(defpackage #:bones-test.wam
  (:use
    #:cl
    #:1am
    #:bones-test.utils
    #:bones.quickutils
    #:bones.wam)
  (:import-from #:bones.wam
    #:with-database
    #:make-database
    #:with-fresh-database
    #:push-logic-frame-with
    #:rule
    #:fact
    #:facts
    #:call
    #:dump-wam-full
    #:?
    #:!
    #:query
    #:query-all)
  (:import-from #:bones.utils
    #:symbolize))

(defpackage #:bones-test.99
  (:use
    #:cl
    #:1am
    #:bones-test.utils
    #:bones.quickutils
    #:bones.wam)
  (:import-from #:bones.wam
    #:with-fresh-database
    #:push-logic-frame-with
    #:rule
    #:fact
    #:facts
    #:call
    #:dump-wam-full
    #:?
    #:!
    #:query
    #:query-all)
  (:import-from #:bones.utils
    #:symbolize))

(defpackage #:bones-test.taop
  (:use
    #:cl
    #:1am
    #:bones-test.utils
    #:bones.quickutils
    #:bones.wam)
  (:import-from #:bones.wam
    #:with-fresh-database
    #:push-logic-frame-with
    #:rule
    #:fact
    #:facts
    #:call
    #:dump-wam-full
    #:?
    #:!
    #:query
    #:query-all)
  (:import-from #:bones.utils
    #:symbolize))

(defpackage #:bones-test.circle
  (:use
    #:cl
    #:1am
    #:bones-test.utils
    #:bones.circle))
