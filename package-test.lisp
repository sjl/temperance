(defpackage #:bones-test
  (:use
    #:cl
    #:5am
    ))

(defpackage #:bones-test.paip
  (:use
    #:cl
    #:5am
    #:bones.quickutils
    #:bones.paip)
  ; kill me
  (:shadowing-import-from #:5am
    #:fail))

(defpackage #:bones-test.wam
  (:use
    #:cl
    #:5am
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
    #:query
    #:query-all)
  (:import-from #:bones.utils
    #:symbolize)
  (:shadowing-import-from #:bones.wam
    #:!))

(defpackage #:bones-test.circle
  (:use :cl :5am :bones.circle))
