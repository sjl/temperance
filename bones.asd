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
               #:cl-arrows
               #:fare-quasiquote-optima
               #:fare-quasiquote-readtable)

  :serial t
  :components ((:file "src/quickutils") ; quickutils package ordering crap
               (:file "package")
               (:module "src"
                :serial t
                :components ((:file "paip")
                             (:file "utils")
                             (:file "circle")
                             (:module "wam"
                              :serial t
                              :components ((:file "constants")
                                           (:file "types")
                                           (:file "cells")
                                           (:file "bytecode")
                                           (:file "wam")
                                           (:file "compiler")
                                           (:file "vm")
                                           (:file "dump")
                                           (:file "ui")))
                             (:file "bones")))))

