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

(defun* push-new-functor! ((wam wam) (functor functor-index))
  (:returns (values heap-cell heap-index))
  "Push a new functor cell onto the heap."
  (wam-heap-push! wam (make-cell-functor functor)))


(defun* bound-reference-p ((wam wam) (address heap-index))
  (:returns boolean)
  "Return whether the cell at `address` is a bound reference."
  (ensure-boolean
    (let ((cell (wam-heap-cell wam address)))
      (and (cell-reference-p cell)
           (not (= (cell-value cell) address))))))

(defun* unbound-reference-p ((wam wam) (address heap-index))
  (:returns boolean)
  "Return whether the cell at `address` is an unbound reference."
  (ensure-boolean
    (let ((cell (wam-heap-cell wam address)))
      (and (cell-reference-p cell)
           (= (cell-value cell) address)))))

(defun* matching-functor-p ((cell heap-cell)
                            (functor functor-index))
  (:returns boolean)
  "Return whether `cell` is a functor cell containing `functor`."
  (ensure-boolean
    (and (cell-functor-p cell)
         (= (cell-functor-index cell) functor))))

(defun* functors-match-p ((functor-cell-1 heap-cell)
                          (functor-cell-2 heap-cell))
  (:returns boolean)
  "Return whether the two functor cells represent the same functor."
  (= (cell-value functor-cell-1)
     (cell-value functor-cell-2)))


(defun* deref ((wam wam) (address heap-index))
  (:returns heap-index)
  "Dereference the address in the WAM to its eventual destination.

  If the address is a variable that's bound to something, that something will be
  looked up (recursively) and the address of whatever it's ultimately bound to
  will be returned.

  "
  (if (bound-reference-p wam address)
    (deref wam (cell-value (wam-heap-cell wam address)))
    address))

(defun* bind! ((wam wam) (address-1 heap-index) (address-2 heap-index))
  (:returns :void)
  "Bind the unbound reference cell to the other.

  `bind!` takes two addresses as arguments.  At least one of these *must* refer
  to an unbound reference cell.  This unbound reference will be bound to point
  at the other address.

  If both addresses refer to unbound references, the direction of the binding is
  chosen arbitrarily.

  "
  (cond
    ((unbound-reference-p wam address-1)
     (setf (wam-heap-cell wam address-1)
           (make-cell-reference address-2)))
    ((unbound-reference-p wam address-2)
     (setf (wam-heap-cell wam address-2)
           (make-cell-reference address-1)))
    (t (error "At least one cell must be an unbound reference when binding.")))
  (values))

(defun* fail! ((wam wam) (reason string))
  (:returns :void)
  "Mark a failure in the WAM."
  (setf (wam-fail wam) t)
  (format *debug-io* "FAIL: ~A~%" reason)
  (break)
  (values))


(defun* unify! ((wam wam) (a1 heap-index) (a2 heap-index))
  (wam-unification-stack-push! wam a1)
  (wam-unification-stack-push! wam a2)
  (setf (wam-fail wam) nil)
  ;; TODO: refactor this horror show.
  (until (or (wam-fail wam)
             (wam-unification-stack-empty-p wam))
    (let ((d1 (deref wam (wam-unification-stack-pop! wam)))
          (d2 (deref wam (wam-unification-stack-pop! wam))))
      (when (not (= d1 d2))
        (let ((cell-1 (wam-heap-cell wam d1))
              (cell-2 (wam-heap-cell wam d2)))
          (if (or (cell-reference-p cell-1)
                  (cell-reference-p cell-2))
            ;; If at least one is a reference, bind them.
            ;;
            ;; We know that any references we see here will be unbound,
            ;; because we deref'ed them above.
            (bind! wam d1 d2)
            ;; Otherwise we're looking at two structures (hopefully, lol).
            (let* ((structure-1-addr (cell-value cell-1)) ; find where they
                   (structure-2-addr (cell-value cell-2)) ; start on the heap
                   (functor-1 (wam-heap-cell wam structure-1-addr)) ; grab the
                   (functor-2 (wam-heap-cell wam structure-2-addr))) ; functors
              (if (functors-match-p functor-1 functor-2)
                ;; If the functors match, push their pairs of arguments onto
                ;; the stack to be unified.
                (loop :with arity = (cdr (wam-functor-lookup wam functor-1))
                      :for i :from 1 :to arity :do
                      (wam-unification-stack-push! wam (+ structure-1-addr i))
                      (wam-unification-stack-push! wam (+ structure-2-addr i)))
                ;; Otherwise we're hosed.
                (fail! wam "Functors don't match in unify!")))))))))


;;;; Query Instructions
(defun* %put-structure ((wam wam)
                        (functor functor-index)
                        (register register-index))
  (:returns :void)
  (->> (push-new-structure! wam)
    (nth-value 1)
    (setf (wam-register wam register)))
  (push-new-functor! wam functor)
  (values))

(defun* %set-variable ((wam wam) (register register-index))
  (:returns :void)
  (->> (push-unbound-reference! wam)
    (nth-value 1)
    (setf (wam-register wam register)))
  (values))

(defun* %set-value ((wam wam) (register register-index))
  (:returns :void)
  (wam-heap-push! wam (wam-register-cell wam register))
  (values))

(defun* %put-variable ((wam wam)
                       (register register-index)
                       (argument register-index))
  (:returns :void)
  (->> (push-unbound-reference! wam)
    (nth-value 1)
    (setf (wam-register wam register))
    (setf (wam-register wam argument)))
  (values))

(defun* %put-value ((wam wam)
                    (register register-index)
                    (argument register-index))
  (:returns :void)
  (setf (wam-register wam argument)
        (wam-register wam register))
  (values))


;;;; Program Instructions
(defun* %get-structure ((wam wam)
                        (functor functor-index)
                        (register register-index))
  (:returns :void)
  (let* ((addr (deref wam (wam-register wam register)))
         (cell (wam-heap-cell wam addr)))
    (cond
      ;; If the register points at a reference cell, we push two new cells onto
      ;; the heap:
      ;;
      ;;     |   N | STR | N+1 |
      ;;     | N+1 | FUN | f/n |
      ;;
      ;; Then we bind this reference cell to point at the new structure and flip
      ;; over to write mode.
      ;;
      ;; It seems a bit confusing that we don't push the rest of the structure
      ;; stuff on the heap after it too.  But that's going to happen in the next
      ;; few instructions (which will be unify-*'s, executed in write mode).
      ((cell-reference-p cell)
       (let ((new-structure-address (nth-value 1 (push-new-structure! wam))))
         (push-new-functor! wam functor)
         (bind! wam addr new-structure-address)
         (setf (wam-mode wam) :write)))

      ;; If the register points at a structure cell, then we look at where that
      ;; cell points (which will be the functor cell for the structure):
      ;;
      ;;     |   N | STR | M   | points at the structure, not necessarily contiguous
      ;;     |       ...       |
      ;;     |   M | FUN | f/2 | the functor (hopefully it matches)
      ;;     | M+1 | ... | ... | pieces of the structure, always contiguous
      ;;     | M+2 | ... | ... | and always right after the functor
      ;;
      ;; If it matches the functor we're looking for, we can proceed.  We set
      ;; the S register to the address of the first subform we need to match
      ;; (M+1 in the example above).
      ;;
      ;; What about if it's a 0-arity functor?  The S register will be set to
      ;; garbage.  But that's okay, because we know the next thing in the stream
      ;; of instructions will be another get-structure and we'll just blow away
      ;; the S register there.
      ((cell-structure-p cell)
       (let* ((functor-addr (cell-value cell))
              (functor-cell (wam-heap-cell wam functor-addr)))
         (if (matching-functor-p functor-cell functor)
           (progn
             (setf (wam-s wam) (1+ functor-addr))
             (setf (wam-mode wam) :read))
           (fail! wam "Functors don't match in get-struct"))))
      (t (fail! wam (format nil "get-struct on a non-ref/struct cell ~A"
                            (cell-aesthetic cell))))))
  (values))

(defun* %unify-variable ((wam wam) (register register-index))
  (:returns :void)
  (ecase (wam-mode wam)
    (:read (setf (wam-register wam register)
                 (wam-s wam)))
    (:write (->> (push-unbound-reference! wam)
              (nth-value 1)
              (setf (wam-register wam register)))))
  (incf (wam-s wam))
  (values))

(defun* %unify-value ((wam wam) (register register-index))
  (:returns :void)
  (ecase (wam-mode wam)
    (:read (unify! wam
                   (wam-register wam register)
                   (wam-s wam)))
    (:write (wam-heap-push! wam (wam-register-cell wam register))))
  (incf (wam-s wam))
  (values))

(defun* %get-variable ((wam wam)
                       (register register-index)
                       (argument register-index))
  (:returns :void)
  (setf (wam-register wam register)
        (wam-register wam argument))
  (values))

(defun* %get-value ((wam wam)
                    (register register-index)
                    (argument register-index))
  (:returns :void)
  (unify! wam
          (wam-register wam register)
          (wam-register wam argument))
  (values))

(defun* %call ((wam wam) (functor functor-index))
  (:returns :void)
  (let ((target (wam-code-label wam functor)))
    (if target
      (progn
        (setf (wam-continuation-pointer wam) ; CP <- next instruction
              (+ (wam-program-counter wam)
                 (instruction-size +opcode-call+))
              (wam-program-counter wam) ; PC <- target 
              target))
      (fail! wam "Tried to call unknown procedure.")))
  (values))

(defun* %proceed ((wam wam))
  (:returns :void)
  (setf (wam-program-counter wam) ; P <- CP
        (wam-continuation-pointer wam))
  (values))


;;;; Running
(defmacro instruction-call (wam instruction code-store pc number-of-arguments)
  "Expand into a call of the appropriate machine instruction.

  `pc` should be a safe place representing the program counter.

  `code-store` should be a safe place representing the instructions.

  "
  `(,instruction ,wam
    ,@(loop :for i :from 1 :to number-of-arguments
            :collect `(aref ,code-store (+ ,pc ,i)))))


(defun run-program (wam functor &optional (step nil))
  (with-slots (code program-counter fail) wam
    (setf program-counter (wam-code-label wam functor))
    (loop
      :while (and (not fail) ; failure
                  (not (= program-counter +code-sentinal+))) ; finished
      :for opcode = (aref code program-counter)
      :do
      (block op
        (when step
          (break "About to execute instruction at ~4,'0X" program-counter))
        (eswitch (opcode)
          (+opcode-get-structure+ (instruction-call wam %get-structure code program-counter 2))
          (+opcode-unify-variable+ (instruction-call wam %unify-variable code program-counter 1))
          (+opcode-unify-value+ (instruction-call wam %unify-value code program-counter 1))
          (+opcode-get-variable+ (instruction-call wam %get-variable code program-counter 2))
          (+opcode-get-value+ (instruction-call wam %get-value code program-counter 2))
          ;; need to skip the PC increment for PROC/CALL
          ;; TODO: this is ugly
          (+opcode-proceed+ (instruction-call wam %proceed code program-counter 0)
                            (return-from op))
          (+opcode-call+ (instruction-call wam %call code program-counter 1)
                         (return-from op)))
        (incf program-counter (instruction-size opcode))
        (when (>= program-counter (fill-pointer code))
          (error "Fell off the end of the program code store!"))))
    (if fail
      (print "FAIL")
      (print "SUCCESS"))))

(defun run-query (wam term &optional (step nil))
  "Compile query `term` and run the instructions on the `wam`.

  Resets the heap, etc before running.

  When `step` is true, break into the debugger before calling the procedure.

  "
  (let ((code (compile-query wam term)))
    (wam-reset! wam)
    (loop
      :with pc = 0 ; local program counter for this hunk of query code
      :for opcode = (aref code pc)
      :do
      (progn
        (eswitch (opcode)
          (+opcode-put-structure+ (instruction-call wam %put-structure code pc 2))
          (+opcode-set-variable+ (instruction-call wam %set-variable code pc 1))
          (+opcode-set-value+ (instruction-call wam %set-value code pc 1))
          (+opcode-put-variable+ (instruction-call wam %put-variable code pc 2))
          (+opcode-put-value+ (instruction-call wam %put-value code pc 2))
          (+opcode-call+
            (when step (break))
            (setf (wam-continuation-pointer wam) +code-sentinal+)
            (run-program wam (aref code (+ pc 1)) step)
            (return)))
        (incf pc (instruction-size opcode))
        (when (>= pc (length code)) ; queries SHOULD always end in a CALL...
          (error "Fell off the end of the query code store!")))))
  (values))


