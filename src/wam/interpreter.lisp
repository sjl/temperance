(in-package #:bones.wam)
(named-readtables:in-readtable :fare-quasiquote)

;;;; Config
(defparameter *break-on-fail* nil)


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
  "Mark a failure in the WAM.

  If `*break-on-fail*` is true, the debugger will be invoked.

  "
  (setf (wam-fail wam) t)
  (when *break-on-fail*
    (break "FAIL: ~A~%" reason))
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


;;;; Instruction Definition
(defmacro define-instruction (name lambda-list &body body)
  `(defun* ,name ,lambda-list
     (:returns :void)
     ,@body
     (values)))

(defmacro define-instructions ((local-name stack-name) lambda-list &body body)
  `(progn
    (macrolet ((%wam-register% (wam register)
                 `(wam-local-register ,wam ,register)))
      (define-instruction ,local-name ,lambda-list
        ,@body))
    (macrolet ((%wam-register% (wam register)
                 `(wam-stack-register ,wam ,register)))
      (define-instruction ,stack-name ,lambda-list
        ,@body))))


;;;; Query Instructions
(define-instruction %put-structure-local
    ((wam wam)
     (functor functor-index)
     (register register-index))
  (->> (push-new-structure! wam)
    (nth-value 1)
    (setf (wam-local-register wam register)))
  (push-new-functor! wam functor))

(define-instructions (%set-variable-local %set-variable-stack)
    ((wam wam)
     (register register-index))
  (->> (push-unbound-reference! wam)
    (nth-value 1)
    (setf (%wam-register% wam register))))

(define-instructions (%set-value-local %set-value-stack)
    ((wam wam)
     (register register-index))
  (wam-heap-push! wam (->> register
                        (%wam-register% wam)
                        (wam-heap-cell wam))))

(define-instructions (%put-variable-local %put-variable-stack)
    ((wam wam)
     (register register-index)
     (argument register-index))
  (->> (push-unbound-reference! wam)
    (nth-value 1)
    (setf (%wam-register% wam register))
    (setf (wam-local-register wam argument))))

(define-instructions (%put-value-local %put-value-stack)
    ((wam wam)
     (register register-index)
     (argument register-index))
  (setf (wam-local-register wam argument)
        (%wam-register% wam register)))


;;;; Program Instructions
(define-instruction %get-structure-local
    ((wam wam)
     (functor functor-index)
     (register register-index))
  (let* ((addr (deref wam (wam-local-register wam register)))
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
                            (cell-aesthetic cell)))))))

(define-instructions (%unify-variable-local %unify-variable-stack)
    ((wam wam)
     (register register-index))
  (ecase (wam-mode wam)
    (:read (setf (%wam-register% wam register)
                 (wam-s wam)))
    (:write (->> (push-unbound-reference! wam)
              (nth-value 1)
              (setf (%wam-register% wam register)))))
  (incf (wam-s wam)))

(define-instructions (%unify-value-local %unify-value-stack)
    ((wam wam)
     (register register-index))
  (ecase (wam-mode wam)
    (:read (unify! wam
                   (%wam-register% wam register)
                   (wam-s wam)))
    (:write (wam-heap-push! wam
                            (->> register
                              (%wam-register% wam)
                              (wam-heap-cell wam)))))
  (incf (wam-s wam)))

(define-instructions (%get-variable-local %get-variable-stack)
    ((wam wam)
     (register register-index)
     (argument register-index))
  (setf (%wam-register% wam register)
        (wam-local-register wam argument)))

(define-instructions (%get-value-local %get-value-stack)
    ((wam wam)
     (register register-index)
     (argument register-index))
  (unify! wam
          (%wam-register% wam register)
          (wam-local-register wam argument)))


;;;; Control Instructions
(define-instruction %call ((wam wam) (functor functor-index))
  (let ((target (wam-code-label wam functor)))
    (if target
      (progn
        (setf (wam-continuation-pointer wam) ; CP <- next instruction
              (+ (wam-program-counter wam)
                 (instruction-size +opcode-call+))
              (wam-program-counter wam) ; PC <- target
              target))
      (fail! wam "Tried to call unknown procedure."))))

(define-instruction %proceed ((wam wam))
  (setf (wam-program-counter wam) ; P <- CP
        (wam-continuation-pointer wam)))

(define-instruction %allocate ((wam wam) (n stack-frame-argcount))
  (setf (wam-environment-pointer wam) ; E <- new E
        (->> wam
          wam-environment-pointer
          (wam-stack-push! wam) ; CE
          (nth-value 1)))
  (wam-stack-push! wam (wam-continuation-pointer wam)) ; CP
  (wam-stack-push! wam n) ; N
  (wam-stack-extend! wam n)) ; Y_n (TODO: this sucks)

(define-instruction %deallocate ((wam wam))
  (setf (wam-program-counter wam)
        (wam-stack-frame-cp wam))
  (wam-stack-pop-environment! wam))


;;;; Running
(defmacro instruction-call (wam instruction code-store pc number-of-arguments)
  "Expand into a call of the appropriate machine instruction.

  `pc` should be a safe place representing the program counter.

  `code-store` should be a safe place representing the instructions.

  "
  `(,instruction ,wam
    ,@(loop :for i :from 1 :to number-of-arguments
            :collect `(aref ,code-store (+ ,pc ,i)))))


(defun extract-query-results (wam goal)
  ;; TODO: rehaul this
  (let ((results (list)))
    (labels ((recur (original result)
               (cond
                 ((and (variable-p original)
                       (not (assoc original results)))
                  (push (cons original
                              (match result
                                (`(,bare-functor) bare-functor)
                                (r r)))
                        results))
                 ((consp original)
                  (recur (car original) (car result))
                  (recur (cdr original) (cdr result)))
                 (t nil))))
      (loop :for argument :in (cdr goal)
            :for a :from 0
            :do (recur argument
                       (extract-thing
                         wam
                         ;; results are stored in local (argument) registers
                         (wam-local-register wam a)))))
    results))


(defun run-program (wam functor &optional (step nil))
  (with-slots (code program-counter fail) wam
    (macrolet ((instruction (inst args &body body)
                 `(progn
                    (instruction-call wam ,inst code program-counter ,args)
                   ,@body)))
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
            ;; Query
            (+opcode-put-structure-local+  (instruction %put-structure-local 2))
            (+opcode-set-variable-local+   (instruction %set-variable-local 1))
            (+opcode-set-variable-stack+   (instruction %set-variable-stack 1))
            (+opcode-set-value-local+      (instruction %set-value-local 1))
            (+opcode-set-value-stack+      (instruction %set-value-stack 1))
            (+opcode-put-variable-local+   (instruction %put-variable-local 2))
            (+opcode-put-variable-stack+   (instruction %put-variable-stack 2))
            (+opcode-put-value-local+      (instruction %put-value-local 2))
            (+opcode-put-value-stack+      (instruction %put-value-stack 2))
            ;; Program
            (+opcode-get-structure-local+  (instruction %get-structure-local 2))
            (+opcode-unify-variable-local+ (instruction %unify-variable-local 1))
            (+opcode-unify-variable-stack+ (instruction %unify-variable-stack 1))
            (+opcode-unify-value-local+    (instruction %unify-value-local 1))
            (+opcode-unify-value-stack+    (instruction %unify-value-stack 1))
            (+opcode-get-variable-local+   (instruction %get-variable-local 2))
            (+opcode-get-variable-stack+   (instruction %get-variable-stack 2))
            (+opcode-get-value-local+      (instruction %get-value-local 2))
            (+opcode-get-value-stack+      (instruction %get-value-stack 2))
            ;; Control
            (+opcode-allocate+             (instruction %allocate 1))
            ;; need to skip the PC increment for PROC/CALL/DEAL
            ;; TODO: this is ugly
            (+opcode-deallocate+   (instruction %deallocate 0 (return-from op)))
            (+opcode-proceed+      (instruction %proceed 0 (return-from op)))
            (+opcode-call+         (instruction %call 1 (return-from op))))
          (incf program-counter (instruction-size opcode))
          (when (>= program-counter (fill-pointer code))
            (error "Fell off the end of the program code store!")))))
    (values)))

(defun run-query (wam term &optional (step nil))
  "Compile query `term` and run the instructions on the `wam`.

  Resets the heap, etc before running.

  When `step` is true, break into the debugger before calling the procedure.

  "
  ;; TODO: dedupe this interpreter code
  (macrolet ((instruction (inst args &body body)
               `(progn
                 (instruction-call wam ,inst code pc ,args)
                 ,@body)))
    (let ((code (compile-query wam term)))
      (when step
        (dump-code-store wam code))
      (wam-reset! wam)
      (loop
        :with pc = 0 ; local program counter for this hunk of query code
        :for opcode = (aref code pc)
        :do
        (progn
          (eswitch (opcode)
            (+opcode-put-structure-local+  (instruction %put-structure-local 2))
            (+opcode-set-variable-local+   (instruction %set-variable-local 1))
            (+opcode-set-variable-stack+   (instruction %set-variable-stack 1))
            (+opcode-set-value-local+      (instruction %set-value-local 1))
            (+opcode-set-value-stack+      (instruction %set-value-stack 1))
            (+opcode-put-variable-local+   (instruction %put-variable-local 2))
            (+opcode-put-variable-stack+   (instruction %put-variable-stack 2))
            (+opcode-put-value-local+      (instruction %put-value-local 2))
            (+opcode-put-value-stack+      (instruction %put-value-stack 2))
            (+opcode-call+
              (when step
                (break "Built query on the heap, about to call program code."))
              (setf (wam-continuation-pointer wam) +code-sentinal+)
              (run-program wam (aref code (+ pc 1)) step)
              (return)))
          (incf pc (instruction-size opcode))
          (when (>= pc (length code)) ; queries SHOULD always end in a CALL...
            (error "Fell off the end of the query code store!"))))))
  (if (wam-fail wam)
    (princ "No.")
    (princ "Yes."))
  (values))


