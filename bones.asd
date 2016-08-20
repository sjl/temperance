(asdf:defsystem #:bones
  :name "bones"
  :description "A logic programming library for Common Lisp."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (#:trivial-types
               #:cl-arrows
               #:policy-cond)

  :in-order-to ((asdf:test-op (asdf:test-op #:bones-test)))

  :serial t
  :components ((:module "vendor"
                :serial t
                :components ((:file "quickutils")))
               (:file "package")
               (:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "circle")
                 (:file "constants")
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
                 (:file "ui")
                 (:file "bones")))))

(asdf:defsystem #:bones-test
  :name "bones-test"
  :description "Test suite for bones."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT/X11"

  :depends-on (#:bones
               #:1am)

  :perform (asdf:test-op
             (op system)
             (uiop:symbol-call :bones-test :run-tests))

  :serial t
  :components ((:file "package-test")
               (:module "test"
                :serial t
                :components ((:file "bones")
                             (:file "utils")
                             (:file "circle")
                             (:file "wam")
                             (:file "99")
                             (:file "taop")))))
