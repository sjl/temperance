(in-package #:bones.utils)

;; TODO: learn setf expanders and do this right.
(defmacro push-if-new (thing list-place)
  `(not (eql ,list-place (pushnew ,thing ,list-place))))

(defun vector-push-extend-all (vector &rest things)
  (loop :for thing :in things :do
        (vector-push-extend thing vector)))
