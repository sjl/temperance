(in-package #:bones.wam)


;;;; WAM
(defun allocate-wam-code (size)
  ;; The WAM bytecode is all stored in this array.  The first
  ;; `+maximum-query-size+` words are reserved for query bytecode, which will
  ;; get loaded in (overwriting the previous query) when making a query.
  ;; Everything after that is for the actual database.
  (make-array (+ +maximum-query-size+ size)
    :initial-element 0
    :element-type 'code-word))

(defun allocate-wam-store (size)
  ;; The main WAM store contains three separate blocks of values:
  ;;
  ;;     [0, +register-count+)        -> the local X_n registers
  ;;     [+stack-start+, +stack-end+) -> the stack
  ;;     [+heap-start+, ...)          -> the heap
  ;;
  ;; `+register-count+` and `+stack-start+` are the same number, and
  ;; `+stack-end+` and `+heap-start+` are the same number as well.
  (make-array (+ +register-count+
                 +stack-limit+
                 size)
    :initial-element (make-cell-null)
    :element-type 'cell))


(defstruct (wam
             (:print-function
              (lambda (wam stream depth)
                (declare (ignore depth))
                (print-unreadable-object
                  (wam stream :type t :identity t)
                  (format stream "an wam"))))
             (:constructor make-wam%))
  (store
    (allocate-wam-store 0)
    :type store
    :read-only t)
  (code
    (allocate-wam-code 0)
    :type (simple-array code-word (*))
    :read-only t)
  (code-labels
    (make-hash-table)
    :read-only t)
  (logic-stack
    nil
    :type list)
  (logic-pool
    nil
    :type list)
  (functors
    (make-array 64
      :fill-pointer 0
      :adjustable t
      :element-type 'functor)
    :type (vector functor)
    :read-only t)
  (unification-stack
    (make-array 16
      :fill-pointer 0
      :adjustable t
      :element-type 'store-index)
    :type (vector store-index)
    :read-only t)
  (trail
    (make-array 64
      :fill-pointer 0
      :adjustable t
      :initial-element 0
      :element-type 'store-index)
    :type (vector store-index)
    :read-only t)

  ;; Unique registers
  (number-of-arguments    0                    :type arity)                ; NARGS
  (subterm                +heap-start+         :type heap-index)           ; S
  (program-counter        0                    :type code-index)           ; P
  (code-pointer           +maximum-query-size+ :type code-index)           ; CODE
  (heap-pointer           (1+ +heap-start+)    :type heap-index)           ; H
  (stack-pointer          +stack-start+        :type stack-index)          ; SP
  (continuation-pointer   0                    :type code-index)           ; CP
  (environment-pointer    +stack-start+        :type environment-pointer)  ; E
  (backtrack-pointer      +stack-start+        :type backtrack-pointer)    ; B
  (cut-pointer            +stack-start+        :type backtrack-pointer)    ; B0
  (heap-backtrack-pointer +heap-start+         :type heap-index)           ; HB

  ;; Other global "registers"
  (fail        nil :type boolean)
  (backtracked nil :type boolean)
  (mode        nil :type (or null (member :read :write))))


(defun* make-wam (&key (store-size (megabytes 10))
                       (code-size (megabytes 1)))
  (:returns wam)
  (make-wam% :code (allocate-wam-code code-size)
             :store (allocate-wam-store store-size)))


;;;; Store
(declaim (inline wam-store-cell (setf wam-store-cell)))


(defun* wam-store-cell ((wam wam) (address store-index))
  (:returns cell)
  "Return the cell at the given address.

  Please don't use this unless you absolutely have to.  Prefer something more
  specific like `wam-heap-cell` else so you've got some extra sanity checking...

  "
  (aref (wam-store wam) address))

(defun* (setf wam-store-cell) ((new-value cell)
                               (wam wam)
                               (address store-index))
  (setf (aref (wam-store wam) address) new-value))


;;;; Heap
;;; The WAM heap is all the memory left in the store after the local registers
;;; and stack have been accounted for.  Because the store is adjustable and the
;;; heap lives at the end of it, the heap can grow if necessary.
;;;
;;; We reserve the first address in the heap as a sentinel, as an "unset" value
;;; for various pointers into the heap.

(declaim (inline wam-heap-pointer-unset-p
                 wam-heap-cell
                 (setf wam-heap-cell)
                 wam-heap-pointer
                 (setf wam-heap-pointer)
                 wam-heap-push!))


(defun* wam-heap-pointer-unset-p ((wam wam) (address heap-index))
  (:returns boolean)
  (declare (ignore wam))
  (= address +heap-start+))


(defun* wam-heap-push! ((wam wam) (cell cell))
  (:returns (values cell heap-index))
  "Push the cell onto the WAM heap and increment the heap pointer.

  Returns the cell and the address it was pushed to.

  "
  (if (>= (wam-heap-pointer wam) +store-limit+) ; todo: respect actual size...
    (error "WAM heap exhausted.")
    (values cell (array-push cell (wam-store wam) (wam-heap-pointer wam)))))


(defun* wam-heap-cell ((wam wam) (address heap-index))
  (:returns cell)
  "Return the heap cell at the given address."
  (when (wam-heap-pointer-unset-p wam address)
    (error "Cannot read from heap address zero."))
  (aref (wam-store wam) address))

(defun* (setf wam-heap-cell) ((new-value cell)
                              (wam wam)
                              (address heap-index))
  (when (wam-heap-pointer-unset-p wam address)
    (error "Cannot write to heap address zero."))
  (setf (aref (wam-store wam) address) new-value))


;;;; Trail
(declaim (inline wam-trail-pointer (setf wam-trail-pointer)))


(defun* wam-trail-pointer ((wam wam))
  (:returns trail-index)
  "Return the current trail pointer of the WAM."
  (fill-pointer (wam-trail wam)))

(defun* (setf wam-trail-pointer) ((new-value trail-index)
                                  (wam wam))
  (setf (fill-pointer (wam-trail wam)) new-value))


(defun* wam-trail-push! ((wam wam) (address store-index))
  (:returns (values store-index trail-index))
  "Push `address` onto the trail.

  Returns the address and the trail address it was pushed to.

  "
  (let ((trail (wam-trail wam)))
    (if (= +trail-limit+ (fill-pointer trail))
      (error "WAM trail exhausted.")
      (values address (vector-push-extend address trail)))))

(defun* wam-trail-pop! ((wam wam))
  (:returns store-index)
  "Pop the top address off the trail and return it."
  (vector-pop (wam-trail wam)))

(defun* wam-trail-value ((wam wam) (address trail-index))
  ;; TODO: can we really not just pop, or is something else gonna do something
  ;; fucky with the trail?
  (:returns store-index)
  "Return the element (a heap index) in the WAM trail at `address`."
  (aref (wam-trail wam) address))

(defun* (setf wam-trail-value) ((new-value store-index)
                                (wam wam)
                                (address trail-index))
  (setf (aref (wam-trail wam) address) new-value))


;;;; Stack
;;; The stack is stored as a fixed-length hunk of the main WAM store array,
;;; between the local register and the heap, with small glitch: we reserve the
;;; first word of the stack (address `+stack-start`) to mean "uninitialized", so
;;; we have a nice sentinel value for the various pointers into the stack.

(declaim (inline assert-inside-stack
                 wam-stack-ensure-size
                 wam-stack-word
                 (setf wam-stack-word)
                 wam-backtrack-pointer-unset-p
                 wam-environment-pointer-unset-p))


(defun* assert-inside-stack ((wam wam) (address store-index))
  (:returns :void)
  (declare (ignorable wam address))
  (policy-cond:policy-cond
    ((>= debug 2)
     (progn
       (assert (<= +stack-start+ address (1- +stack-end+)) ()
         "Cannot access stack cell at address ~X (outside the stack range ~X to ~X)"
         address +stack-start+ +stack-end+)
       (assert (not (= +stack-start+ address)) ()
         "Cannot access stack address zero.")))
    ((>= safety 1)
     (when (not (< +stack-start+ address +stack-end+))
       (error "Stack bounds crossed.  Game over.")))
    (t nil)) ; wew lads
  (values))

(defun* wam-stack-ensure-size ((wam wam) (address stack-index))
  (:returns :void)
  "Ensure the WAM stack is large enough to be able to write to `address`."
  (assert-inside-stack wam address)
  (values))


(defun* wam-stack-word ((wam wam) (address stack-index))
  (:returns stack-word)
  "Return the stack word at the given address."
  (assert-inside-stack wam address)
  (aref (wam-store wam) address))

(defun* (setf wam-stack-word) ((new-value stack-word)
                               (wam wam)
                               (address stack-index))
  (assert-inside-stack wam address)
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
;;;     | B0 |
;;;     | N  |
;;;     | Y0 |
;;;     | .. |
;;;     | Yn |
;;;     |NEXT| <-- fill-pointer

(declaim (inline wam-stack-frame-ce
                 wam-stack-frame-cp
                 wam-stack-frame-cut
                 wam-stack-frame-n
                 wam-stack-frame-arg
                 (setf wam-stack-frame-arg)
                 wam-stack-frame-size))

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

(defun* wam-stack-frame-cut
    ((wam wam)
     &optional
     ((e environment-pointer)
      (wam-environment-pointer wam)))
  (:returns backtrack-pointer)
  (wam-stack-word wam (+ 2 e)))

(defun* wam-stack-frame-n
    ((wam wam)
     &optional
     ((e environment-pointer)
      (wam-environment-pointer wam)))
  (:returns stack-frame-argcount)
  (wam-stack-word wam (+ 3 e)))


(defun* wam-stack-frame-arg
    ((wam wam)
     (n register-index)
     &optional
     ((e environment-pointer)
      (wam-environment-pointer wam)))
  (:returns cell)
  (wam-stack-word wam (+ 4 n e)))

(defun* (setf wam-stack-frame-arg) ((new-value cell)
                                    (wam wam)
                                    (n register-index)
                                    &optional ((e environment-pointer)
                                               (wam-environment-pointer wam)))
  (setf (wam-stack-word wam (+ e 4 n))
        new-value))


(defun* wam-stack-frame-size
    ((wam wam)
     &optional
     ((e environment-pointer)
      (wam-environment-pointer wam)))
  (:returns stack-frame-size)
  "Return the size of the stack frame starting at environment pointer `e`."
  (+ (wam-stack-frame-n wam e) 4))


;;; Choice point frames are laid out like so:
;;;
;;;         |PREV|
;;;       0 | N  | number of arguments          <-- backtrack-pointer
;;;       1 | CE | continuation environment
;;;       2 | CP | continuation pointer
;;;       3 | CB | previous choice point
;;;       4 | BP | next clause
;;;       5 | TR | trail pointer
;;;       6 | H  | heap pointer
;;;       7 | CC | saved cut pointer
;;;       8 | A0 |
;;;         | .. |
;;;     8+n | An |
;;;         |NEXT| <-- environment-pointer
;;;
;;; This is a bit different than the book.  We stick the args at the end of the
;;; frame instead of the beginning so it's easier to retrieve the other values.

(declaim (inline wam-stack-choice-n
                 wam-stack-choice-ce
                 wam-stack-choice-cp
                 wam-stack-choice-cb
                 wam-stack-choice-cc
                 wam-stack-choice-bp
                 wam-stack-choice-tr
                 wam-stack-choice-h
                 wam-stack-choice-arg
                 (setf wam-stack-choice-arg)
                 wam-stack-choice-size))

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

(defun* wam-stack-choice-cc
    ((wam wam)
     &optional
     ((b backtrack-pointer)
      (wam-backtrack-pointer wam)))
  (:returns backtrack-pointer)
  (wam-stack-word wam (+ b 7)))


(defun* wam-stack-choice-arg
    ((wam wam)
     (n arity)
     &optional
     ((b backtrack-pointer)
      (wam-backtrack-pointer wam)))
  (:returns cell)
  (wam-stack-word wam (+ b 8 n)))

(defun* (setf wam-stack-choice-arg) ((new-value cell)
                                     (wam wam)
                                     (n arity)
                                     &optional ((b backtrack-pointer)
                                                (wam-backtrack-pointer wam)))
  (setf (wam-stack-word wam (+ b 8 n))
        new-value))


(defun* wam-stack-choice-size
    ((wam wam)
     &optional
     ((b backtrack-pointer)
      (wam-backtrack-pointer wam)))
  (:returns stack-choice-size)
  "Return the size of the choice frame starting at backtrack pointer `b`."
  (+ (wam-stack-choice-n wam b) 8))


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
  (setf (wam-heap-pointer wam) (1+ +heap-start+)))

(defun* wam-truncate-trail! ((wam wam))
  (setf (fill-pointer (wam-trail wam)) 0))

(defun* wam-truncate-unification-stack! ((wam wam))
  (setf (fill-pointer (wam-unification-stack wam)) 0))

(defun* wam-reset-local-registers! ((wam wam))
  (fill (wam-store wam) (make-cell-null) :start 0 :end +register-count+))

(defun* wam-reset! ((wam wam))
  (wam-truncate-heap! wam)
  (wam-truncate-trail! wam)
  (wam-truncate-unification-stack! wam)
  (policy-cond:policy-if (>= debug 2)
    (wam-reset-local-registers! wam)
    nil) ; fuck it
  (setf (wam-program-counter wam) 0
        (wam-continuation-pointer wam) 0
        (wam-environment-pointer wam) +stack-start+
        (wam-backtrack-pointer wam) +stack-start+
        (wam-cut-pointer wam) +stack-start+
        (wam-heap-backtrack-pointer wam) +heap-start+
        (wam-backtracked wam) nil
        (wam-fail wam) nil
        (wam-subterm wam) +heap-start+
        (wam-mode wam) nil))


;;;; Code
(defun* retrieve-instruction (code-store (address code-index))
  "Return the full instruction at the given address in the code store."
  (make-array (instruction-size (aref code-store address))
    :displaced-to code-store
    :displaced-index-offset address
    :adjustable nil
    :element-type 'code-word))

(defun* wam-code-label ((wam wam)
                        (functor functor-index))
  (:returns (or null code-index))
  (gethash functor (wam-code-labels wam)))

(defun* (setf wam-code-label) ((new-value code-index)
                               (wam wam)
                               (functor symbol)
                               (arity arity))
  ;; Note that this takes a functor/arity and not a cons.
  (setf (gethash (wam-ensure-functor-index wam (cons functor arity))
                 (wam-code-labels wam))
        new-value))


(defun* wam-load-query-code! ((wam wam)
                              (query-code query-code-holder))
  (:returns :void)
  (setf (subseq (wam-code wam) 0) query-code)
  (values))


;;;; Logic Stack
(defstruct logic-frame
  (start 0 :type code-index)
  (final nil :type boolean)
  (predicates (make-hash-table) :type hash-table))


(defun* wam-logic-pool-release ((wam wam) (frame logic-frame))
  (:returns :void)
  (with-slots (start final predicates) frame
    (clrhash predicates)
    (setf start 0 final nil))
  (push frame (wam-logic-pool wam))
  (values))

(defun* wam-logic-pool-request ((wam wam))
  (:returns logic-frame)
  (or (pop (wam-logic-pool wam))
      (make-logic-frame)))


(defun* wam-current-logic-frame ((wam wam))
  (:returns (or null logic-frame))
  (first (wam-logic-stack wam)))

(defun* wam-logic-stack-empty-p ((wam wam))
  (:returns boolean)
  (not (wam-current-logic-frame wam)))


(defun* wam-logic-open-p ((wam wam))
  (:returns boolean)
  (let ((frame (wam-current-logic-frame wam)))
    (and frame (not (logic-frame-final frame)))))

(defun* wam-logic-closed-p ((wam wam))
  (:returns boolean)
  (not (wam-logic-open-p wam)))


(defun* wam-push-logic-frame! ((wam wam))
  (:returns :void)
  (assert (wam-logic-closed-p wam) ()
    "Cannot push logic frame unless the logic stack is closed.")
  (let ((frame (wam-logic-pool-request wam)))
    (setf (logic-frame-start frame)
          (wam-code-pointer wam))
    (push frame (wam-logic-stack wam)))
  (values))

(defun* wam-pop-logic-frame! ((wam wam))
  (:returns :void)
  (with-slots (logic-stack) wam
    (assert logic-stack ()
      "Cannot pop logic frame from an empty logic stack.")
    (assert (logic-frame-final (first logic-stack)) ()
      "Cannot pop unfinalized logic frame.")
    (let ((frame (pop logic-stack)))
      (setf (wam-code-pointer wam)
            (logic-frame-start frame))
      (loop :for label :being :the hash-keys :of (logic-frame-predicates frame)
            :do (remhash label (wam-code-labels wam)))
      (wam-logic-pool-release wam frame)))
  (values))


(defun* assert-label-not-already-compiled ((wam wam) clause label)
  (assert (not (wam-code-label wam label))
      ()
    "Cannot add clause ~S because its predicate has preexisting compiled code."
    clause))

(defun* wam-logic-frame-add-clause! ((wam wam) clause)
  (assert (wam-logic-open-p wam) ()
    "Cannot add clause ~S without an open logic stack frame."
    clause)
  (multiple-value-bind (functor arity) (find-predicate clause)
    (let ((label (wam-ensure-functor-index wam (cons functor arity))))
      (assert-label-not-already-compiled wam clause label)
      (with-slots (predicates)
          (wam-current-logic-frame wam)
        (enqueue clause (gethash-or-init label predicates (make-queue))))))
  (values))


(defun* wam-finalize-logic-frame! ((wam wam))
  (assert (wam-logic-open-p wam) ()
    "There is no logic frame waiting to be finalized.")
  (with-slots (predicates final)
      (wam-current-logic-frame wam)
    (loop :for clauses :being :the hash-values :of predicates
          ;; circular dep on the compiler here, ugh.
          :do (compile-rules wam (queue-contents clauses)))
    (setf final t))
  (values))


;;;; Registers
;;; The WAM has two types of registers:
;;;
;;; * Local/temporary/arguments registers live at the beginning of the WAM
;;;   memory store.
;;;
;;; * Stack/permanent registers live on the stack, and need some extra math to
;;;   find their location.
;;;
;;; Registers are typically denoted by their "register index", which is just
;;; their number.  Hoever, the bytecode needs to be able to distinguish between
;;; local and stack registers.  To do this we just make separate opcodes for
;;; each kind.  This is ugly, but it lets us figure things out at compile time
;;; instead of runtime, and register references happen A LOT at runtime.
;;;
;;; As for the CONTENTS of registers: a register (regardless of type) always
;;; contains a cell.  The book is maddeningly unclear on this in a bunch of
;;; ways.  I will list them here so maybe you can feel a bit of my suffering
;;; through these bytes of text.
;;;
;;; The first thing the book says about registers is "registers have the same
;;; format as heap cells".  Okay, fine.  The *very next diagram* shows "register
;;; assignments" that appear to put things that are very much *not* heap cells
;;; into registers!
;;;
;;; After a bit of puttering you realize that the diagram is referring only to
;;; the compilation, not what's *actually* stored in these registers at runtime.
;;; You move on and see some pseudocode that contains `X_i <- HEAP[H]` which
;;; confirms that his original claim was accurate, and registers are actually
;;; (copies of) heap cells.  Cool.
;;;
;;; Then you move on and see the definition of `deref(a : address)` and note
;;; that it takes an *address* as an argument.  On the next page you see
;;; `deref(X_i)` and wait what the fuck, a register is an *address* now?  You
;;; scan down the page and see `HEAP[H] <- X_i` which means no wait it's a cell
;;; again.
;;;
;;; After considering depositing your laptop into the nearest toilet and
;;; becoming a sheep farmer, you conclude a few things:
;;;
;;; 1. The book's code won't typecheck.
;;; 2. The author is playing fast and loose with `X_i` -- sometimes it seems to
;;;    be used as an address, sometimes as a cell.
;;; 3. The author never bothers to nail down exactly what is inside the fucking
;;;    things, which is a problem because of #2.
;;;
;;; If you're like me (painfully unlucky), you took a wild guess and decided to
;;; implement registers as containing *addresses*, i.e., indexes into the
;;; heap, figuring that if you were wrong it would soon become apparent.
;;;
;;; WELL it turns out that you can get all the way to CHAPTER FIVE with
;;; registers implemented as addresses, at which point you hit a wall and need
;;; to spend a few hours refactoring a giant chunk of your code and writing
;;; angry comments in your source code.
;;;
;;; Hopefully I can save someone else this misery by leaving you with this:
;;;     ____  _____________________________________  _____    ___    ____  ______   ______________    __   _____
;;;    / __ \/ ____/ ____/  _/ ___/_  __/ ____/ __ \/ ___/   /   |  / __ \/ ____/  / ____/ ____/ /   / /  / ___/
;;;   / /_/ / __/ / / __ / / \__ \ / / / __/ / /_/ /\__ \   / /| | / /_/ / __/    / /   / __/ / /   / /   \__ \
;;;  / _, _/ /___/ /_/ // / ___/ // / / /___/ _, _/___/ /  / ___ |/ _, _/ /___   / /___/ /___/ /___/ /______/ /
;;; /_/ |_/_____/\____/___//____//_/ /_____/_/ |_|/____/  /_/  |_/_/ |_/_____/   \____/_____/_____/_____/____/

(declaim (inline wam-local-register
                 (setf wam-local-register)
                 wam-stack-register
                 (setf wam-stack-register)))

(defun* wam-local-register ((wam wam) (register register-index))
  (:returns cell)
  "Return the value stored in the WAM local register with the given index."
  (aref (wam-store wam) register))

(defun* (setf wam-local-register) ((new-value cell)
                                   (wam wam)
                                   (register register-index))
  (setf (aref (wam-store wam) register) new-value))


(defun* wam-stack-register ((wam wam) (register register-index))
  (:returns cell)
  "Return the value stored in the WAM stack register with the given index."
  (wam-stack-frame-arg wam register))

(defun* (setf wam-stack-register) ((new-value cell)
                                   (wam wam)
                                   (register register-index))
  (setf (wam-stack-frame-arg wam register) new-value))


(defun* wam-s-cell ((wam wam))
  "Retrieve the cell the S register is pointing at.

  If S is unbound, throws an error.

  "
  (let ((s (wam-subterm wam)))
    (if (wam-heap-pointer-unset-p wam s)
      (error "Cannot dereference unbound S register.")
      (wam-heap-cell wam s))))


;;;; Functors
;;; Functors are stored in an adjustable array.  Cells refer to a functor using
;;; the functor's address in this array.

(declaim (inline wam-functor-lookup
                 wam-functor-symbol
                 wam-functor-arity))

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
(defun* wam-unification-stack-push! ((wam wam) (address store-index))
  (vector-push-extend address (wam-unification-stack wam)))

(defun* wam-unification-stack-pop! ((wam wam))
  (:returns store-index)
  (vector-pop (wam-unification-stack wam)))

(defun* wam-unification-stack-empty-p ((wam wam))
  (:returns boolean)
  (zerop (length (wam-unification-stack wam))))
