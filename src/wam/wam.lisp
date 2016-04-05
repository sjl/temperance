(in-package #:bones.wam)

;;;; WAM
(defclass wam ()
  ((heap
     :initform (make-array 1024
                           :fill-pointer 0
                           :adjustable t
                           :initial-element (make-cell-null)
                           :element-type 'heap-cell)
     :reader wam-heap
     :documentation "The actual heap (stack).")
   (code
     :initform (make-array 1024
                           :fill-pointer 0
                           :adjustable t
                           :initial-element 0
                           :element-type 'code-word)
     :reader wam-code
     :documentation "The code store.")
   (functors
     :initform (make-array 64
                           :fill-pointer 0
                           :adjustable t
                           :element-type 'functors)
     :accessor wam-functors
     :documentation "The array of functors in this WAM.")
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
   (program-counter
     :accessor wam-program-counter
     :initform 0
     :type 'code-index
     :documentation "The Program Counter for the WAM code store.")
   (mode
     :accessor wam-mode
     :initform nil
     :type (or null (member :read :write))
     :documentation "Current unification mode (:READ or :WRITE (or NIL)).")))


(defun make-wam ()
  (make-instance 'wam))


;;;; Heap
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


;;;; Code
(defun* retrieve-instruction (code-store (address code-index))
  "Return the full instruction at the given address in the code store."
  (make-array (instruction-size (aref code-store address))
              :displaced-to code-store
              :displaced-index-offset address
              :adjustable nil
              :element-type 'code-word))


(defun* wam-code-word ((wam wam) (address code-index))
  (:returns code-word)
  "Return the word at the given address in the code store."
  (aref (wam-code wam) address))

(defun (setf wam-code-word) (word wam address)
  (setf (aref (wam-code wam) address) word))


(defun* wam-code-instruction ((wam wam) (address code-index))
  "Return the full instruction at the given address in the code store."
  (retrieve-instruction (wam-code wam) address))


(defun* wam-code-push-word! ((wam wam) (word code-word))
  "Push the given word into the code store and return its new address."
  (:returns code-index)
  (with-slots (code) wam
    (if (= +code-limit+ (fill-pointer code))
      (error "WAM code store exhausted.")
      (vector-push-extend word code))))

(defun* wam-code-push! ((wam wam) (opcode opcode) &rest (arguments code-word))
  "Push the given instruction into the code store and return its new address.

  The address will be the address of the start of the instruction (i.e. the
  address of the opcode).

  "
  (:returns code-index)
  (assert (= (length arguments)
             (1- (instruction-size opcode)))
          (arguments)
          "Cannot push opcode ~A with ~D arguments ~S, it requires exactly ~D."
          (opcode-name opcode)
          (length arguments)
          arguments
          (instruction-size opcode))
  (prog1
      (wam-code-push-word! wam opcode)
    (dolist (arg arguments)
      (wam-code-push-word! wam arg))))


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
;;; Functors are stored in an adjustable array.  Cells refer to a functor using
;;; the functor's address in this array.

(defun* wam-ensure-functor-index ((wam wam) (functor functor))
  (:returns functor-index)
  "Return the index of the functor in the WAM's functor table.

  If the functor is not already in the table it will be added.

  "
  (with-slots (functors) wam
    (or (position functor functors :test #'equal)
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
