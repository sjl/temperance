(asdf:defsystem :temperance.test
  :description "Test suite for Temperance."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT/X11"

  :depends-on (:temperance
               :1am)

  :perform (asdf:test-op
             (op system)
             (uiop:symbol-call :temperance.test :run-tests))

  :serial t
  :components ((:file "package.test")
               (:module "test"
                :serial t
                :components ((:file "temperance")
                             (:file "utils")
                             (:file "circle")
                             (:file "wam")
                             (:file "99")
                             (:file "taop")))))
