(in-package #:bones.wam)

(defun* put-structure ((wam wam)
                       (functor symbol)
                       (arity arity)
                       (register register-index))
  (:returns :void)
  (let ((structure-cell (make-cell-structure (1+ (wam-heap-pointer wam))))
        (functor-cell (make-cell-functor
                        (wam-ensure-functor-index wam functor)
                        arity)))
    (wam-heap-push! wam structure-cell)
    (wam-heap-push! wam functor-cell)
    (setf (wam-register wam register) structure-cell))
  (values))

(defun* set-variable ((wam wam) (register register-index))
  (:returns :void)
  (let ((cell (make-cell-reference (wam-heap-pointer wam))))
    (wam-heap-push! wam cell)
    (setf (wam-register wam register) cell))
  (values))

(defun* set-value ((wam wam) (register register-index))
  (:returns :void)
  (wam-heap-push! wam (wam-register wam register))
  (values))

