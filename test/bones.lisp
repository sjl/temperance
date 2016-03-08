(in-package #:bones-test)

(def-suite :bones)
(in-suite :bones)

(test hello-works
  (is (equal 0 (1- (hello)))))
