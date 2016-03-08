(defpackage #:bones-test
  (:use #:cl #:5am
        #:bones))

(defpackage #:bones-test.paip
  (:use #:cl #:5am
        #:bones.utils
        #:bones.paip)
  (:import-from #:bones.paip)
  ; kill me
  (:shadowing-import-from #:5am #:fail))
