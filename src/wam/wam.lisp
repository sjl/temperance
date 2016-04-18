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
     ;; The WAM bytecode is all stored in this array.  The first
     ;; `+maximum-query-size+` words are reserved for query bytecode, which will
     ;; get loaded in (overwriting the previous query) when making a query.
     ;; Everything after that is for the actual database.
     :initform (make-array (+ +maximum-query-size+ 1024)
                 :adjustable t
                 :fill-pointer +maximum-query-size+
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
     :reader wam-local-registers
     :initform (make-array +register-count+
                 ;; Initialize to the last element in the heap for debugging.
                 ;; todo: don't do this
                 :initial-element (1- +heap-limit+)
                 :element-type 'heap-index)
     :documentation "An array of the local X_i registers.")
   (stack
     :reader wam-stack
     :initform (make-array 1024
                 :adjustable t
                 :fill-pointer 0
                 ;; Initialize to the last element in the heap for debugging.
                 ;; todo: don't do this
                 :initial-element (1- +heap-limit+)
                 :element-type 'stack-word)
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


(defun* wam-stack-word ((wam wam) (address stack-index))
  (:returns stack-index)
  "Return the stack word at the given address."
  (aref (wam-stack wam) address))

(defun (setf wam-stack-word) (new-value wam address)
  (setf (aref (wam-stack wam) address) new-value))


(defun* wam-stack-frame-ce
    ((wam wam)
     &optional
     ((e environment-pointer)
      (wam-environment-pointer wam)))
  (:returns environment-pointer)
  (wam-stack-word wam e))

(defun* wam-stack-frame-cp
    ((wam wam)
     &optional
     ((e environment-pointer)
      (wam-environment-pointer wam)))
  (:returns continuation-pointer)
  (wam-stack-word wam (1+ e)))

(defun* wam-stack-frame-n
    ((wam wam)
     &optional
     ((e environment-pointer)
      (wam-environment-pointer wam)))
  (:returns stack-frame-argcount)
  (wam-stack-word wam (+ 2 e)))

(defun* wam-stack-frame-arg
    ((wam wam)
     (n register-index)
     &optional
     ((e environment-pointer) (wam-environment-pointer wam)))
  (:returns heap-index)
  (wam-stack-word wam (+ 3 n e)))

(defun (setf wam-stack-frame-arg)
    (new-value wam n &optional (e (wam-environment-pointer wam)))
  (setf (wam-stack-word wam (+ e 3 n))
        new-value))

(defun* wam-stack-frame-arg-cell
    ((wam wam)
     (n register-index)
     &optional
     ((e environment-pointer)
      (wam-environment-pointer wam)))
  (:returns heap-cell)
  (wam-heap-cell wam (wam-stack-frame-arg wam n e)))


(defun* wam-stack-frame-size
    ((wam wam)
     &optional
     ((e environment-pointer)
      (wam-environment-pointer wam)))
  (:returns stack-frame-size)
  "Return the size of the stack frame starting at environment pointer `e`."
  (+ (wam-stack-frame-n wam e) 3))


(defun* wam-stack-push! ((wam wam) (word stack-word))
  (:returns (values stack-word stack-index))
  "Push the word onto the WAM stack and increment the stack pointer.

  Returns the word and the address it was pushed to.

  "
  (with-slots (stack) wam
    (if (= +stack-limit+ (fill-pointer stack))
      (error "WAM stack exhausted.")
      (values word (vector-push-extend word stack)))))

(defun* wam-stack-extend! ((wam wam) (words integer))
  (:returns :void)
  "Extend the WAM stack by the given number of words.

  Each word is initialized to 0.

  "
  ;; TODO: this sucks, fix it
  (with-slots (stack) wam
    (repeat words
      (if (= +stack-limit+ (fill-pointer stack))
        (error "WAM stack exhausted.")
        (vector-push-extend 0 stack))))
  (values))

(defun* wam-stack-pop-environment! ((wam wam))
  "Pop an environment (stack frame) off the WAM stack."
  (let ((frame-size (wam-stack-frame-size wam)))
    (with-slots (stack environment-pointer) wam
      (setf environment-pointer (wam-stack-frame-ce wam)) ; E <- CE
      (decf (fill-pointer stack) frame-size)))) ; its fine


;;;; Resetting
(defun* wam-truncate-heap! ((wam wam))
  (setf (fill-pointer (wam-heap wam)) 0))

(defun* wam-truncate-stack! ((wam wam))
  (setf (fill-pointer (wam-stack wam)) 0))

(defun* wam-reset-local-registers! ((wam wam))
  (loop :for i :from 0 :below +register-count+ :do
        (setf (wam-local-register wam i)
              (1- +heap-limit+)))
  (setf (wam-s wam) nil))

(defun* wam-reset! ((wam wam))
  (wam-truncate-heap! wam)
  (wam-truncate-stack! wam)
  (wam-reset-local-registers! wam)
  (setf (wam-program-counter wam) 0
        (wam-continuation-pointer wam) 0
        (wam-environment-pointer wam) 0
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
          (1- (instruction-size opcode)))
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


(defun* wam-load-query-code! ((wam wam) query-code)
  (:returns :void)
  (when (> (length query-code) +maximum-query-size+)
    (error "WAM query store exhausted."))
  ;; TODO: there must be a better way to do this
  (loop :for word :across query-code
        :for addr :from 0
        :do (setf (aref (wam-code wam) addr)
                  word))
  (values))


;;;; Registers
;;; The WAM has two types of registers.  A register (regardless of type) always
;;; contains an index into the heap (basically a pointer to a heap cell).
;;;
;;; Local/temporary/arguments registers live in a small, fixed, preallocated
;;; array called `registers` in the WAM object.
;;;
;;; Stack/permanent registers live on the stack, and need some extra math to
;;; find their location.
;;;
;;; Registers are typically denoted by their "register index", which is just
;;; their number.  Hoever, the bytecode needs to be able to distinguish between
;;; local and stack registers.  To do this we just make separate opcodes for
;;; each kind.  This is ugly, but it lets us figure things out at compile time
;;; instead of runtime, and register references happen A LOT at runtime.

(defun* wam-local-register ((wam wam) (register register-index))
  (:returns heap-index)
  "Return the value of the WAM local register with the given index."
  (aref (wam-local-registers wam) register))

(defun (setf wam-local-register) (new-value wam register)
  (setf (aref (wam-local-registers wam) register) new-value))


(defun* wam-stack-register ((wam wam) (register register-index))
  (:returns heap-index)
  "Return the value of the WAM stack register with the given index."
  (wam-stack-frame-arg wam register))

(defun (setf wam-stack-register) (new-value wam register)
  (setf (wam-stack-frame-arg wam register) new-value))


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
