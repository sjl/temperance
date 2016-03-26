(in-package #:bones.wam)

;;;; WAM
(defclass wam ()
  ((heap
     :initform (make-array 32
                           :initial-element (make-cell-null)
                           :element-type 'heap-cell)
     :reader wam-heap
     :documentation "The actual heap (stack).")
   (heap-pointer
     :initform 0
     :accessor wam-heap-pointer
     :documentation "The index of the first free cell on the heap (stack).")
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
                           :initial-element (make-cell-null)
                           :element-type 'heap-cell)
     :documentation "An array of the X_i registers.")))


(defun make-wam ()
  (make-instance 'wam))


;;;; Heap
;;; The WAM heap is a fixed-length array of cells and a heap pointer.
;;;
;;; TODO: Consider using an adjustable array.  There must still be a max size
;;; because you can only index so many addresses with N bits.

(defun* wam-heap-push! ((wam wam) (cell heap-cell))
  (:returns heap-cell)
  "Push the cell onto the WAM heap and increment the heap pointer.

  Returns the cell.

  "
  (with-slots (heap heap-pointer) wam
    (setf (aref heap heap-pointer) cell)
    (incf heap-pointer)
    cell))


(defun* wam-heap-cell ((wam wam) (address heap-index))
  (:returns heap-cell)
  "Return the heap cell at the given address."
  (aref (wam-heap wam) address))

(defun (setf wam-heap-cell) (new-value wam address)
  (setf (aref (wam-heap wam) address) new-value))


;;;; Registers
;;; WAM registers are implemented as an array of a fixed number of registers.

(defun* wam-register ((wam wam) (register register-index))
  (:returns heap-cell)
  "Return the WAM register with the given index."
  (aref (wam-registers wam) register))

(defun (setf wam-register) (new-value wam register)
  (setf (aref (wam-registers wam) register) new-value))


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

