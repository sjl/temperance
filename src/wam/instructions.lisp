(in-package #:bones.wam)

;;;; Utilities
(defun* push-unbound-reference! ((wam wam))
  (:returns (values heap-cell heap-index))
  "Push a new unbound reference cell onto the heap."
  (wam-heap-push! wam (make-cell-reference (wam-heap-pointer wam))))

(defun* push-new-structure! ((wam wam))
  (:returns (values heap-cell heap-index))
  "Push a new structure cell onto the heap.

  The structure cell's value will point at the next address, so make sure you
  push something there too!

  "
  (wam-heap-push! wam (make-cell-structure (1+ (wam-heap-pointer wam)))))

(defun* push-new-functor! ((wam wam) (functor symbol) (arity arity))
  (:returns (values heap-cell heap-index))
  "Push a new functor cell onto the heap.

  If the functor isn't already in the functor table it will be added.

  "
  (wam-heap-push! wam (make-cell-functor
                        (wam-ensure-functor-index wam functor)
                        arity)))


(defun* bound-reference-p ((address heap-index) (cell heap-cell))
  (:returns boolean)
  "Return whether `cell` is a bound reference, assuming it lives at `address`."
  (ensure-boolean
    (and (cell-reference-p cell)
         (not (= (cell-value cell) address)))))

(defun* unbound-reference-p ((address heap-index) (cell heap-cell))
  (:returns boolean)
  "Return whether `cell` is an unbound reference, assuming it lives at `address`."
  (ensure-boolean
    (and (cell-reference-p cell)
         (= (cell-value cell) address))))

(defun* matching-functor-p ((wam wam)
                            (cell heap-cell)
                            (functor symbol)
                            (arity arity))
  (:returns boolean)
  "Return whether `cell` is a functor cell of `functor`/`arity`."
  (ensure-boolean
    (and (cell-functor-p cell)
         (= arity (cell-functor-arity cell))
         (eql functor
              (wam-functor-lookup wam (cell-functor-index cell))))))


(defun* deref ((wam wam) (address heap-index))
  (:returns heap-index)
  "Dereference the address in the WAM to its eventual destination.

  If the address is a variable that's bound to something, that something will be
  looked up (recursively) and the address of whatever it's ultimately bound to
  will be returned.

  "
  (let ((cell (wam-heap-cell wam address)))
    (if (bound-reference-p address cell)
      (deref wam (cell-value cell))
      address)))


(defun* bind! ((wam wam) (address heap-index) (target heap-index))
  "Bind the reference cell at `address` to `target`.

  The reference cell must be unbound to begin with.
  TODO: are we sure about this?

  `target` doesn't necessarily need to exist yet.
  TODO: this seems dangerous...

  "
  (assert (unbound-reference-p address
                               (wam-heap-cell wam address))
          ()
          "Cannot bind address ~D because it is not an unbound reference."
          address)
  (setf (wam-heap-cell wam address)
        (make-cell-reference target)))

(defun* fail! ((wam wam))
  "Mark a failure in the WAM."
  (setf (wam-fail wam) t))


(defun* unify ((wam wam) (a1 heap-index) (a2 heap-index))
  nil
  )


;;;; Query Instructions
(defun* %put-structure ((wam wam)
                        (functor symbol)
                        (arity arity)
                        (register register-index))
  (:returns :void)
  (setf (wam-register wam register)
        (nth-value 1 (push-new-structure! wam)))
  (push-new-functor! wam functor arity)
  (values))

(defun* %set-variable ((wam wam) (register register-index))
  (:returns :void)
  (setf (wam-register wam register)
        (nth-value 1 (push-unbound-reference! wam)))
  (values))

(defun* %set-value ((wam wam) (register register-index))
  (:returns :void)
  (wam-heap-push! wam (wam-register-cell wam register))
  (values))


;;;; Program Instructions
(defun* %get-structure ((wam wam)
                        (functor symbol)
                        (arity arity)
                        (register register-index))
  (:returns :void)
  (let* ((addr (deref wam (wam-register wam register)))
         (cell (wam-heap-cell wam addr)))
    (cond
      ;; If the register points at a reference cell
      ((cell-reference-p cell)
       (bind! wam addr (wam-heap-pointer wam))
       (push-new-structure! wam)
       (push-new-functor! wam functor arity)
       (setf (wam-mode wam) :write))
      ;; If the register points at a structure cell
      ((cell-structure-p cell)
       (let* ((target-addr (cell-value cell))
              (target (wam-heap-cell wam target-addr)))
         (if (matching-functor-p wam target functor arity)
           (progn
             (setf (wam-s wam) (1+ target-addr))
             (setf (wam-mode wam) :read))
           (fail! wam))))
      (t (fail! wam))))
  (values))

(defun* %unify-variable ((wam wam) (register register-index))
  (:returns :void)
  (ecase (wam-mode wam)
    (:read (setf (wam-register wam register)
                 (wam-s-cell wam)))
    (:write (setf (wam-register wam register)
                  (nth-value 1 (push-unbound-reference! wam)))))
  (incf (wam-s wam))
  (values))

(defun* %unify-value ((wam wam) (register register-index))
  (:returns :void)
  (ecase (wam-mode wam)
    (:read (unify wam
                  (cell-value (wam-register wam register))
                  (wam-s wam)))
    (:write (wam-heap-push! wam (wam-register wam register))))
  (incf (wam-s wam))
  (values))

