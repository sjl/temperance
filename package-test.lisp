(defpackage #:bones-test
  (:use
    #:cl
    #:5am
    #:bones))

(defpackage #:bones-test.paip
  (:use
    #:cl
    #:5am
    #:bones.quickutils
    #:bones.paip)
  ; kill me
  (:shadowing-import-from #:5am #:fail))
