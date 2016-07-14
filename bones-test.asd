(asdf:defsystem #:bones-test
  :description "Test suite for bones."
  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT/X11"
  :depends-on (#:bones
               #:1am)
  :serial t
  :components ((:file "package-test")
               (:module "test"
                :serial t
                :components ((:file "bones")
                             (:file "circle")
                             (:file "paip")
                             (:file "wam")))))

