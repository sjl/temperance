(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "bones-test"))


(in-package #:bones-test)
(run!)
