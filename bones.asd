(asdf:defsystem #:bones
  :name "bones"
  :description "A logic programming library for Common Lisp."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (#:defstar
               #:optima
               #:trivial-types
               #:fare-quasiquote-optima
               #:fare-quasiquote-readtable)

  :serial t
  :components ((:file "src/utils") ; quickutils package ordering crap
               (:file "package")
               (:module "src"
                :components ((:file "paip")
                             (:file "wam")
                             ; (:file "paip-compiled")
                             (:file "bones")))))

