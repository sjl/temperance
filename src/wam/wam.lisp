(in-package #:bones.wam)

;;;; WAM
(declaim
  ;; Inline all these struct accessors, otherwise things get REAL slow.
  (inline wam-store
          wam-code
          wam-functors
          wam-code-labels
          wam-fail
          wam-backtracked
          wam-unification-stack
          wam-trail
          wam-number-of-arguments
          wam-subterm
          wam-program-counter
          wam-continuation-pointer
          wam-environment-pointer
          wam-backtrack-pointer
          wam-heap-backtrack-pointer
          wam-mode))

(defstruct (wam
             (:print-function
              (lambda (wam stream depth)
                (declare (ignore depth))
                (print-unreadable-object
                  (wam stream :type t :identity t)
                  (format stream "an wam")))))
  (store
    ;; The main WAM store contains three separate blocks of values:
    ;;
    ;;     [0, +register-count+)        -> the local X_n registers
    ;;     [+stack-start+, +stack-end+) -> the stack
    ;;     [+heap-start+, ...)          -> the heap
    ;;
    ;; `+register-count+` and `+stack-start+` are the same number, and
    ;; `+stack-end+` and `+heap-start+` are the same number as well.
    (make-array (+ +register-count+ ; TODO: make all these configurable per-WAM
                   +stack-limit+
                   4096)
      :fill-pointer +stack-end+
      :adjustable t
      :initial-element (make-cell-null)
      :element-type 'cell)
    :type (vector cell)
    :read-only t)
  (code
    ;; The WAM bytecode is all stored in this array.  The first
    ;; `+maximum-query-size+` words are reserved for query bytecode, which will
    ;; get loaded in (overwriting the previous query) when making a query.
    ;; Everything after that is for the actual database.
    (make-array (+ +maximum-query-size+ 1024)
      :adjustable t
      :fill-pointer +maximum-query-size+
      :initial-element 0
      :element-type 'code-word)
    :type (vector code-word)
    :read-only t)
  (functors
    (make-array 64
      :fill-pointer 0
      :adjustable t
      :element-type 'functor)
    :type (vector functor)
    :read-only t)
  (code-labels
    (make-hash-table)
    :read-only t)
  (unification-stack
    (make-array 16
      :fill-pointer 0
      :adjustable t
      :element-type 'heap-index)
    :type (vector heap-index)
    :read-only t)
  (trail
    (make-array 64
      :fill-pointer 0
      :adjustable t
      :initial-element 0
      :element-type 'heap-index)
    :type (vector heap-index)
    :read-only t)

  ;; Unique registers
  (number-of-arguments    0             :type arity)                ; NARGS
  (subterm                nil           :type (or null heap-index)) ; S
  (program-counter        0             :type code-index)           ; P
  (stack-pointer          +stack-start+ :type stack-index)          ; SP
  (continuation-pointer   0             :type code-index)           ; CP
  (environment-pointer    +stack-start+ :type environment-pointer)  ; E
  (backtrack-pointer      +stack-start+ :type backtrack-pointer)    ; B
  (heap-backtrack-pointer +heap-start+  :type heap-index)           ; HB

  ;; Other global "registers"
  (fail        nil :type boolean)
  (backtracked nil :type boolean)
  (mode        nil :type (or null (member :read :write))))


;;;; Heap
;;; TODO: Should we privilege heap address 0 to mean "unset" so we have a good
;;; sentinal value for HB, S, etc?

(defun* wam-heap-push! ((wam wam) (cell cell))
  (:returns (values cell heap-index))
  "Push the cell onto the WAM heap and increment the heap pointer.

  Returns the cell and the address it was pushed to.

  "
  (let ((store (wam-store wam)))
    (if (= +store-limit+ (fill-pointer store))
      (error "WAM heap exhausted.")
      (values cell (vector-push-extend cell store)))))

(defun* wam-heap-pointer ((wam wam))
  (:returns heap-index)
  "Return the current heap pointer of the WAM."
  (fill-pointer (wam-store wam)))

(defun (setf wam-heap-pointer) (new-value wam)
  (setf (fill-pointer (wam-store wam)) new-value))


(defun* wam-heap-cell ((wam wam) (address heap-index))
  (:returns cell)
  "Return the heap cell at the given address."
  (aref (wam-store wam) address))

(defun (setf wam-heap-cell) (new-value wam address)
  (setf (aref (wam-store wam) address) new-value))


;;;; Trail
(defun* wam-trail-pointer ((wam wam))
  (:returns trail-index)
  "Return the current trail pointer of the WAM."
  (fill-pointer (wam-trail wam)))

(defun (setf wam-trail-pointer) (new-value wam)
  (setf (fill-pointer (wam-trail wam)) new-value))


(defun* wam-trail-push! ((wam wam) (address heap-index))
  (:returns (values heap-index trail-index))
  "Push `address` onto the trail.

  Returns the address and the trail address it was pushed to.

  "
  (let ((trail (wam-trail wam)))
    (if (= +trail-limit+ (fill-pointer trail))
      (error "WAM trail exhausted.")
      (values address (vector-push-extend address trail)))))

(defun* wam-trail-pop! ((wam wam))
  (:returns heap-index)
  "Pop the top address off the trail and return it."
  (vector-pop (wam-trail wam)))

(defun* wam-trail-value ((wam wam) (address trail-index))
  ;; TODO: can we really not just pop, or is something else gonna do something
  ;; fucky with the trail?
  (:returns heap-index)
  "Return the element (a heap index) in the WAM trail at `address`."
  (aref (wam-trail wam) address))


;;;; Stack
;;; The stack is stored as a fixed-length hunk of the main WAM store array,
;;; between the local register and the heap, with small glitch: we reserve the
;;; first word of the stack (address `+stack-start`) to mean "uninitialized", so
;;; we have a nice sentinal value for the various pointers into the stack.

(declaim (inline wam-stack-word))

(defun assert-inside-stack (wam address action)
  (declare (ignore wam))
  (assert (<= +stack-start+ address (1- +stack-end+)) ()
    "Cannot ~A stack cell at address ~X (outside the stack range ~X to ~X)"
    action address +stack-start+ +stack-end+)
  (assert (not (= +stack-start+ address)) ()
    "Cannot ~A stack address zero."
    action))

(defun* wam-stack-ensure-size ((wam wam) (address stack-index))
  (:returns :void)
  "Ensure the WAM stack is large enough to be able to write to `address`."
  (assert-inside-stack wam address "write")
  (values))


(defun* wam-stack-word ((wam wam) (address stack-index))
  (:returns stack-word)
  "Return the stack word at the given address."
  (assert-inside-stack wam address "read")
  (aref (wam-store wam) address))

(defun (setf wam-stack-word) (new-value wam address)
  (assert-inside-stack wam address "write")
  (setf (aref (wam-store wam) address) new-value))


(defun* wam-backtrack-pointer-unset-p
  ((wam wam)
   &optional
   ((backtrack-pointer backtrack-pointer)
    (wam-backtrack-pointer wam)))
  (:returns boolean)
  (= backtrack-pointer +stack-start+))

(defun* wam-environment-pointer-unset-p
  ((wam wam)
   &optional
   ((environment-pointer environment-pointer)
    (wam-environment-pointer wam)))
  (:returns boolean)
  (= environment-pointer +stack-start+))


;;; Stack frames are laid out like so:
;;;
;;;     |PREV|
;;;     | CE | <-- environment-pointer
;;;     | CP |
;;;     | N  |
;;;     | Y0 |
;;;     | .. |
;;;     | Yn |
;;;     |NEXT| <-- fill-pointer
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
     ((e environment-pointer)
      (wam-environment-pointer wam)))
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
  (:returns cell)
  (wam-heap-cell wam (wam-stack-frame-arg wam n e)))


(defun* wam-stack-frame-size
    ((wam wam)
     &optional
     ((e environment-pointer)
      (wam-environment-pointer wam)))
  (:returns stack-frame-size)
  "Return the size of the stack frame starting at environment pointer `e`."
  (+ (wam-stack-frame-n wam e) 3))


;;; Choice point frames are laid out like so:
;;;
;;;         |PREV|
;;;       0 | N  | <-- backtrack-pointer
;;;       1 | CE |
;;;       2 | CP | This is a bit different than the book.  We stick the
;;;       3 | CB | arguments at the end of the frame instead of the beginning,
;;;       4 | BP | so it's easier to retrieve the other values.
;;;       5 | TR |
;;;       6 | H  |
;;;       7 | A0 |
;;;         | .. |
;;;     7+n | An |
;;;         |NEXT| <-- fill-pointer

(defun* wam-stack-choice-n
    ((wam wam)
     &optional
     ((b backtrack-pointer)
      (wam-backtrack-pointer wam)))
  (:returns arity)
  (wam-stack-word wam b))

(defun* wam-stack-choice-ce
    ((wam wam)
     &optional
     ((b backtrack-pointer)
      (wam-backtrack-pointer wam)))
  (:returns environment-pointer)
  (wam-stack-word wam (+ b 1)))

(defun* wam-stack-choice-cp
    ((wam wam)
     &optional
     ((b backtrack-pointer)
      (wam-backtrack-pointer wam)))
  (:returns continuation-pointer)
  (wam-stack-word wam (+ b 2)))

(defun* wam-stack-choice-cb
    ((wam wam)
     &optional
     ((b backtrack-pointer)
      (wam-backtrack-pointer wam)))
  (:returns backtrack-pointer)
  (wam-stack-word wam (+ b 3)))

(defun* wam-stack-choice-bp
    ((wam wam)
     &optional
     ((b backtrack-pointer)
      (wam-backtrack-pointer wam)))
  (:returns continuation-pointer)
  (wam-stack-word wam (+ b 4)))

(defun* wam-stack-choice-tr
    ((wam wam)
     &optional
     ((b backtrack-pointer)
      (wam-backtrack-pointer wam)))
  (:returns trail-index)
  (wam-stack-word wam (+ b 5)))

(defun* wam-stack-choice-h
    ((wam wam)
     &optional
     ((b backtrack-pointer)
      (wam-backtrack-pointer wam)))
  (:returns heap-index)
  (wam-stack-word wam (+ b 6)))


(defun* wam-stack-choice-arg
    ((wam wam)
     (n arity)
     &optional
     ((b backtrack-pointer)
      (wam-backtrack-pointer wam)))
  (:returns heap-index)
  (wam-stack-word wam (+ b 7 n)))

(defun (setf wam-stack-choice-arg)
    (new-value wam n &optional (b (wam-backtrack-pointer wam)))
  (setf (wam-stack-word wam (+ b 7 n))
        new-value))

(defun* wam-stack-choice-arg-cell
    ((wam wam)
     (n arity)
     &optional
     ((b backtrack-pointer)
      (wam-backtrack-pointer wam)))
  (:returns cell)
  (wam-heap-cell wam (wam-stack-choice-arg wam n b)))


(defun* wam-stack-choice-size
    ((wam wam)
     &optional
     ((b backtrack-pointer)
      (wam-backtrack-pointer wam)))
  (:returns stack-choice-size)
  "Return the size of the choice frame starting at backtrack pointer `b`."
  (+ (wam-stack-choice-n wam b) 7))


(defun* wam-stack-top ((wam wam))
  (:returns stack-index)
  "Return the top of the stack.

  This is the first place it's safe to overwrite in the stack.

  "
  ;; The book is wrong here -- it looks up the "current frame size" to
  ;; determine where the next frame should start, but on the first allocation
  ;; there IS no current frame so it looks at garbage.  Fuckin' great.
  (let ((e (wam-environment-pointer wam))
        (b (wam-backtrack-pointer wam)))
    (cond
      ((and (wam-backtrack-pointer-unset-p wam b)
            (wam-environment-pointer-unset-p wam e)) ; first allocation 
       (1+ +stack-start+))
      ((> e b) ; the last thing on the stack is a frame
       (+ e (wam-stack-frame-size wam e)))
      (t ; the last thing on the stack is a choice point
       (+ b (wam-stack-choice-size wam b))))))


;;;; Resetting
(defun* wam-truncate-heap! ((wam wam))
  (setf (fill-pointer (wam-store wam)) +heap-start+))

(defun* wam-truncate-trail! ((wam wam))
  (setf (fill-pointer (wam-trail wam)) 0))

(defun* wam-truncate-unification-stack! ((wam wam))
  (setf (fill-pointer (wam-unification-stack wam)) 0))

(defun* wam-reset-local-registers! ((wam wam))
  (loop :for i :from 0 :below +register-count+ :do
        (setf (wam-local-register wam i) (make-cell-null)))
  (setf (wam-subterm wam) nil))

(defun* wam-reset! ((wam wam))
  (wam-truncate-heap! wam)
  (wam-truncate-trail! wam)
  (wam-truncate-unification-stack! wam)
  (wam-reset-local-registers! wam)
  (setf (wam-program-counter wam) 0
        (wam-continuation-pointer wam) 0
        (wam-environment-pointer wam) +stack-start+
        (wam-backtrack-pointer wam) +stack-start+
        (wam-heap-backtrack-pointer wam) +heap-start+
        (wam-backtracked wam) nil
        (wam-fail wam) nil
        (wam-subterm wam) nil
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

;; Note that this takes a functor/arity and not a cons.
(defun (setf wam-code-label) (new-value wam functor arity)
  (setf (gethash (wam-ensure-functor-index wam (cons functor arity))
                 (wam-code-labels wam))
        new-value))


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
;;; Local/temporary/arguments registers live at the beginning of the WAM memory
;;; store.
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
  (:returns (or (eql 0) heap-index))
  "Return the value of the WAM local register with the given index."
  (aref (wam-store wam) register))

(defun (setf wam-local-register) (new-value wam register)
  (setf (aref (wam-store wam) register) new-value))


(defun* wam-stack-register ((wam wam) (register register-index))
  (:returns (or (eql 0) heap-index))
  "Return the value of the WAM stack register with the given index."
  (wam-stack-frame-arg wam register))

(defun (setf wam-stack-register) (new-value wam register)
  (setf (wam-stack-frame-arg wam register) new-value))


(defun* wam-s-cell ((wam wam))
  "Retrieve the cell the S register is pointing at.

  If S is unbound, throws an error.

  "
  (let ((s (wam-subterm wam)))
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
  (let ((functors (wam-functors wam)))
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
