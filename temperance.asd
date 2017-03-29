(asdf:defsystem :temperance
  :description "A logic programming library for Common Lisp."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "1.0.0"

  :depends-on (:policy-cond)

  :in-order-to ((asdf:test-op (asdf:test-op :temperance.test)))

  :serial t
  :components ((:module "vendor"
                :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
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
                 (:file "ui")))))
