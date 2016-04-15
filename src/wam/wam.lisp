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
                           :adjustable t
                           :fill-pointer 0
                           :initial-element 0
                           :element-type 'code-word)
     :reader wam-code
     :documentation "The code store.")
   (functors
     :initform (make-array 64
                           :fill-pointer 0
                           :adjustable t
                           :element-type 'functor)
     :accessor wam-functors
     :documentation "The array of functors in this WAM.")
   (code-labels
     :initform (make-hash-table)
     :accessor wam-code-labels
     :documentation "The mapping of functor indices -> code store addresses.")
   (registers
     :reader wam-registers
     :initform (make-array +register-count+
                           ;; Initialize to the last element in the heap for
                           ;; debugging purposes.
                           ;; todo: don't do this
                           :initial-element (1- +heap-limit+)
                           :element-type 'heap-index)
     :documentation "An array of the X_i registers.")
   (stack
     :reader wam-stack
     :initform (make-array 1024
                           :adjustable t
                           :fill-pointer 0
                           ;; Initialize to the last element in the heap for
                           ;; debugging purposes.
                           ;; todo: don't do this
                           :initial-element (1- +heap-limit+)
                           :element-type 'stack-cell)
     :documentation "The local stack for storing stack frames.")
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
     :type code-index
     :documentation "The Program Counter into the WAM code store.")
   (continuation-pointer
     :accessor wam-continuation-pointer
     :initform 0
     :type code-index
     :documentation "The Continuation Pointer into the WAM code store.")
   (environment-pointer
     :accessor wam-environment-pointer
     :initform 0
     :type stack-index
     :documentation "The Environment Pointer into the WAM stack.")
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


;;;; Stack
;;; Stack frames are laid out like so:
;;;
;;;     |PREV|
;;;     | CE | <-- environment-pointer
;;;     | CP |
;;;     | N  |
;;;     | Y0 |
;;;     | .. |
;;;     | YN |
;;;     |NEXT| <-- fill-pointer

(defun* wam-stack-pointer ((wam wam))
  (:returns stack-index)
  "Return the current stack pointer of the WAM."
  (fill-pointer (wam-stack wam)))


(defun* wam-stack-cell ((wam wam) (address stack-index))
  (:returns stack-index)
  "Return the stack cell at the given address."
  (aref (wam-stack wam) address))

(defun (setf wam-stack-cell) (new-value wam address)
  (setf (aref (wam-stack wam) address) new-value))


(defun* wam-stack-frame-ce ((wam wam)
                            &optional
                            ((e environment-pointer) (wam-environment-pointer wam)))
  (:returns environment-pointer)
  (wam-stack-cell wam e))

(defun* wam-stack-frame-cp ((wam wam)
                            &optional
                            ((e environment-pointer) (wam-environment-pointer wam)))
  (:returns continuation-pointer)
  (wam-stack-cell wam (1+ e)))

(defun* wam-stack-frame-n ((wam wam)
                            &optional
                            ((e environment-pointer) (wam-environment-pointer wam)))
  (:returns register-index)
  (wam-stack-cell wam (+ 2 e)))

(defun* wam-stack-frame-arg ((wam wam)
                             (n register-index)
                             &optional
                             ((e environment-pointer) (wam-environment-pointer wam)))
  (:returns heap-index)
  (wam-stack-cell wam (+ 3 n e)))

(defun* wam-stack-frame-arg-cell ((wam wam)
                                  (n register-index)
                                  &optional
                                  ((e environment-pointer) (wam-environment-pointer wam)))
  (:returns heap-cell)
  (wam-heap-cell wam (wam-stack-frame-arg wam n e)))


(defun* wam-stack-frame-size ((wam wam)
                              &optional
                              ((e environment-pointer) (wam-environment-pointer wam)))
  (:returns (integer 3 1024)) ; TODO: Type this better
  "Return the size of the stack frame starting at environment pointer `e`."
  (+ (wam-stack-frame-n wam e) 3))


(defun* wam-stack-push! ((wam wam) (cell stack-cell))
  (:returns (values stack-cell stack-index))
  "Push the cell onto the WAM stack and increment the stack pointer.

  Returns the cell and the address it was pushed to.

  "
  (with-slots (stack) wam
    (if (= +stack-limit+ (fill-pointer stack))
      (error "WAM stack exhausted.")
      (values cell (vector-push-extend cell stack)))))

(defun* wam-stack-pop-environment! ((wam wam))
  "Pop an environment (stack frame) off the WAM stack."
  (let ((frame-size (wam-stack-frame-size wam)))
    (with-slots (stack environment-pointer) wam
      (decf environment-pointer frame-size)
      (decf (fill-pointer stack) frame-size))))


;;;; Resetting
(defun* wam-truncate-heap! ((wam wam))
  (setf (fill-pointer (wam-heap wam)) 0))

(defun* wam-truncate-stack! ((wam wam))
  (setf (fill-pointer (wam-stack wam)) 0))

(defun* wam-reset-registers! ((wam wam))
  (loop :for i :from 0 :below +register-count+ :do
        (setf (wam-register wam i)
              (1- +heap-limit+)))
  (setf (wam-s wam) nil))

(defun* wam-reset! ((wam wam))
  (wam-truncate-heap! wam)
  (wam-truncate-stack! wam)
  (wam-reset-registers! wam)
  (setf (wam-program-counter wam) 0
        (wam-continuation-pointer wam) 0
        (wam-fail wam) nil
        (wam-mode wam) nil))


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


(defun* code-push-word! ((store (array code-word))
                         (word code-word))
  "Push the given word into the code store and return its new address."
  (:returns code-index)
  (vector-push-extend word store))

(defun* code-push-instruction! ((store (array code-word))
                                (opcode opcode)
                                &rest (arguments code-word))
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
      (code-push-word! store opcode)
    (dolist (arg arguments)
      (code-push-word! store arg))))


(defun* wam-code-label ((wam wam)
                        (functor functor-index))
  (:returns (or null code-index))
  (gethash functor (wam-code-labels wam)))

(defun (setf wam-code-label) (new-value wam functor)
  (setf (gethash functor (wam-code-labels wam)) new-value))


;;;; Registers
;;; WAM registers are implemented as an array of a fixed number of registers.
;;; A register contains the address of a cell in the heap.

(defun* wam-register ((wam wam) (register register-index))
  (:returns heap-index)
  "Return the value of the WAM register with the given index."
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
  (:returns functor)
  "Return the functor with the given index in the WAM."
  (aref (wam-functors wam) functor-index))

(defun* wam-functor-symbol ((wam wam) (functor-index functor-index))
  (:returns symbol)
  "Return the symbol of the functor with the given index in the WAM."
  (car (wam-functor-lookup wam functor-index)))

(defun* wam-functor-arity ((wam wam) (functor-index functor-index))
  (:returns arity)
  "Return the arity of the functor with the given index in the WAM."
  (cdr (wam-functor-lookup wam functor-index)))


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
