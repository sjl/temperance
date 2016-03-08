(in-package #:bones-test.paip)

(def-suite :bones.paip)
(in-suite :bones.paip)

(test foo-works
  (is (equal 0 (1- (foo)))))
