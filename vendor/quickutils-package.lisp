(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "TEMPERANCE.QUICKUTILS")
    (defpackage "TEMPERANCE.QUICKUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use :cl))))

(in-package "TEMPERANCE.QUICKUTILS")

;; need to define this here so sbcl will shut the hell up about it being
;; undefined when compiling quickutils.lisp.  computers are trash.
(defparameter *utilities* nil)

