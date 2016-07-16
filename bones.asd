(asdf:defsystem #:bones
  :name "bones"
  :description "A logic programming library for Common Lisp."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (#:optima
               #:trivial-types
               #:cl-arrows
               #:policy-cond
               #:fare-quasiquote-optima
               #:fare-quasiquote-readtable)

  :serial t
  :components ((:file "src/quickutils") ; quickutils package ordering crap
               (:file "package")
               (:module "src"
                :serial t
                :components
                ((:file "paip")
                 (:file "utils")
                 (:file "circle")
                 (:module "wam"
                  :serial t
                  :components ((:file "constants")
                               (:file "types")
                               (:file "bytecode")
                               (:file "wam")
                               (:module "compiler"
                                :serial t
                                :components ((:file "0-data")
                                             (:file "1-parsing")
                                             (:file "2-register-allocation")
                                             (:file "3-flattening")
                                             (:file "4-tokenization")
                                             (:file "5-precompilation")
                                             (:file "6-optimization")
                                             (:file "7-rendering")
                                             (:file "8-ui")))
                               (:file "vm")
                               (:file "dump")
                               (:file "ui")))
                 (:file "bones")))))

