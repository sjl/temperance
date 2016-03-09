(defpackage #:bones
  (:use #:cl)
  (:export #:hello))

(defpackage #:bones.paip
  (:use #:cl #:defstar #:bones.utils)
  (:export #:unify
           #:fail #:no-bindings
           #:*check-occurs*))

