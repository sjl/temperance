(in-package #:bones.wam)

;;;; WAM
(defclass wam ()
  ((heap
     :initform (make-array 1024
                           :fill-pointer 0
                           :initial-element (make-cell-null)
                           :element-type 'heap-cell)
     :reader wam-heap
     :documentation "The actual heap (stack).")
   (functors
     :initform (make-array 16
                           :fill-pointer 0
                           :adjustable t
                           :element-type 'symbol)
     :accessor wam-functors
     :documentation "The array of functor symbols in this WAM.")
   (registers
     :reader wam-registers
     :initform (make-array +register-count+
                           ;; Initialize to the last element in the heap for
                           ;; debugging purposes.
                           :initial-element (1- +heap-limit+)
                           :element-type 'heap-index)
     :documentation "An array of the X_i registers.")
   (fail
     :accessor wam-fail
     :initform nil
     :type boolean
     :documentation "The failure register.")
   (unification-stack
     :reader wam-unification-stack
     :initform (make-array 16
                           :fill-pointer 0
                           :adjustable t
                           :element-type 'heap-index)
     :documentation "The unification stack.")
   (s
     :accessor wam-s
     :initform nil
     :type (or null heap-index)
     :documentation "The S register (address of next subterm to match).")
   (mode
     :accessor wam-mode
     :initform nil
     :type (or null (member :read :write))
     :documentation "Current unification mode (:READ or :WRITE (or NIL)).")))


(defun make-wam ()
  (make-instance 'wam))


;;;; Heap
;;; The WAM heap is a fixed-length array of cells and a heap pointer.
;;;
;;; TODO: Consider using an adjustable array.  There must still be a max size
;;; because you can only index so many addresses with N bits.
(defun* wam-heap-push! ((wam wam) (cell heap-cell))
  (:returns (values heap-cell heap-index))
  "Push the cell onto the WAM heap and increment the heap pointer.

  Returns the cell and the address it was pushed to.

  "
  (with-slots (heap) wam
    (if (= +heap-limit+ (fill-pointer heap))
      (error "WAM heap exhausted.")
      (values cell (vector-push-extend cell heap)))))

(defun* wam-heap-pointer ((wam wam))
  (:returns heap-index)
  "Return the current heap pointer of the WAM."
  (fill-pointer (wam-heap wam)))


(defun* wam-heap-cell ((wam wam) (address heap-index))
  (:returns heap-cell)
  "Return the heap cell at the given address."
  (aref (wam-heap wam) address))

(defun (setf wam-heap-cell) (new-value wam address)
  (setf (aref (wam-heap wam) address) new-value))


;;;; Registers
;;; WAM registers are implemented as an array of a fixed number of registers.
;;; A register contains the address of a cell in the heap.

(defun* wam-register ((wam wam) (register register-index))
  (:returns heap-cell)
  "Return the WAM register with the given index."
  (aref (wam-registers wam) register))

(defun (setf wam-register) (new-value wam register)
  (setf (aref (wam-registers wam) register) new-value))

(defun* wam-register-cell ((wam wam) (register register-index))
  (:returns heap-cell)
  "Return the heap cell `register` is pointing at."
  (->> register
    (wam-register wam)
    (wam-heap-cell wam)))

(defun* wam-s-cell ((wam wam))
  "Retrieve the cell the S register is pointing at.

  If S is unbound, throws an error.

  "
  (let ((s (wam-s wam)))
    (if (null s)
      (error "Cannot dereference unbound S register.")
      (wam-heap-cell wam s))))


;;;; Functors
;;; Functors are symbols stored in an adjustable array.  Cells refer to
;;; a functor using the functor's address in this array.
;;;
;;; TODO: Limit the number of functors based on the number of addressable
;;; functors in the functor cell index bits.

(defun* wam-ensure-functor-index ((wam wam) (functor symbol))
  (:returns functor-index)
  "Return the index of the functor in the WAM's functor table.

  If the functor is not already in the table it will be added.

  "
  (with-slots (functors) wam
    (or (position functor functors)
        (vector-push-extend functor functors))))

(defun* wam-functor-lookup ((wam wam) (functor-index functor-index))
  (:returns symbol)
  "Return the symbol for the functor with the given index in the WAM."
  (aref (wam-functors wam) functor-index))


;;;; Unification Stack
(defun* wam-unification-stack-push! ((wam wam) (address heap-index))
  (:returns :void)
  (vector-push-extend address (wam-unification-stack wam))
  (values))

(defun* wam-unification-stack-pop! ((wam wam))
  (:returns heap-index)
  (vector-pop (wam-unification-stack wam)))

(defun* wam-unification-stack-empty-p ((wam wam))
  (:returns boolean)
  (zerop (length (wam-unification-stack wam))))
