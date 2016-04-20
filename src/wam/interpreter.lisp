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
;;; These macros are a pair of real greasy bastards.
;;;
;;; Basically the issue is that there exist two separate types of registers:
;;; local registers and stack registers.  The process of retrieving the contents
;;; of a register is different for each type.
;;;
;;; Certain machine instructions take a register as an argument and do something
;;; with it.  Because the two register types require different access methods,
;;; the instruction needs to know what kind of register it's dealing with.
;;;
;;; One possible way to solve this would be to encode whether this is
;;; a local/stack register in the register argument itself (e.g. with a tag
;;; bit).  This would work, and a previous version of the code did that, but
;;; it's not ideal.  It turns out we know the type of the register at compile
;;; time, so requiring a mask/test at run time for every register access is
;;; wasteful.
;;;
;;; Instead we use an ugly, but fast, solution.  For every instruction that
;;; takes a register argument we make TWO opcodes instead of just one.  The
;;; first is the "-local" variant of the instruction, which treats its register
;;; argument as a local register.  The second is the "-stack" variant.  When we
;;; compile we can just pick the appropriate opcode, and now we no longer need
;;; a runtime test for every single register assignment.
;;;
;;; To make the process of defining these two "variants" we have these two
;;; macros.  `define-instruction` (singular) is just a little sugar around
;;; `defun*`, for those instructions that don't deal with arguments.
;;;
;;; `define-instructions` (plural) is the awful one.  You pass it a pair of
;;; symbols for the two variant names.  Two functions will be defined, both with
;;; the same body, with the symbol `%wam-register%` macroletted to the
;;; appropriate access code.  So in the body, instead of using
;;; `(wam-{local/argument}-register wam register)` you just use
;;; `(%wam-register% wam register)` and it'll do the right thing.

(defmacro define-instruction (name lambda-list &body body)
  "Define an instruction function.

  This is just syntactic sugar over `defun*` that will add the `(returns :void)`
  declaration for you, and also append a `(values)` to the end of the body to
  make sure it actually does return void.

  "
  `(defun* ,name ,lambda-list
     (:returns :void)
     ,@body
     (values)))

(defmacro define-instructions ((local-name stack-name) lambda-list &body body)
  "Define a local/stack pair of instructions."
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
(define-instruction %get-structure-local ((wam wam)
                                          (functor functor-index)
                                          (register register-index))
  (with-accessors ((mode wam-mode) (s wam-subterm)) wam
    (let* ((addr (deref wam (wam-local-register wam register)))
           (cell (wam-heap-cell wam addr)))
      (cond
        ;; If the register points at a reference cell, we push two new cells onto
        ;; the heap:
        ;;
        ;;     |   N | STR | N+1 |
        ;;     | N+1 | FUN | f/n |
        ;;     |     |     |     | <- S
        ;;
        ;; Then we bind this reference cell to point at the new structure, set the
        ;; S register to point beneath it and flip over to write mode.
        ;;
        ;; It seems a bit confusing that we don't push the rest of the structure
        ;; stuff on the heap after it too.  But that's going to happen in the next
        ;; few instructions (which will be unify-*'s, executed in write mode).
        ((cell-reference-p cell)
         (let ((structure-address (nth-value 1 (push-new-structure! wam)))
               (functor-address (push-new-functor! wam functor)))
           (bind! wam addr structure-address)
           (setf mode :write
                 s (1+ functor-address))))

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
             (setf s (1+ functor-addr)
                   mode :read)
             (fail! wam "Functors don't match in get-struct"))))
        (t (fail! wam (format nil "get-struct on a non-ref/struct cell ~A"
                              (cell-aesthetic cell))))))))

(define-instructions (%unify-variable-local %unify-variable-stack)
    ((wam wam)
     (register register-index))
  (ecase (wam-mode wam)
    (:read (setf (%wam-register% wam register)
                 (wam-subterm wam)))
    (:write (->> (push-unbound-reference! wam)
              (nth-value 1)
              (setf (%wam-register% wam register)))))
  (incf (wam-subterm wam)))

(define-instructions (%unify-value-local %unify-value-stack)
    ((wam wam)
     (register register-index))
  (ecase (wam-mode wam)
    (:read (unify! wam
                   (%wam-register% wam register)
                   (wam-subterm wam)))
    (:write (wam-heap-push! wam
                            (->> register
                              (%wam-register% wam)
                              (wam-heap-cell wam)))))
  (incf (wam-subterm wam)))

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
  ;; Use the slots directly here for speed.  I know this sucks.  I'm sorry.
  (with-slots (stack environment-pointer) wam
    (let* ((old-e environment-pointer)
           (new-e (+ old-e (wam-stack-frame-size wam old-e))))
      (wam-stack-ensure-size! wam (+ new-e 3 n))
      (setf (aref stack new-e) old-e ; E
            (aref stack (+ new-e 1) (wam-continuation-pointer wam)) ; CP
            (aref stack (+ new-e 2) n) ; N
            environment-pointer new-e)))) ; E <- new-e

(define-instruction %deallocate ((wam wam))
  (setf (wam-program-counter wam)
        (wam-stack-frame-cp wam)
        (wam-environment-pointer wam)
        (wam-stack-frame-ce wam)))


;;;; Running
(defmacro instruction-call (wam instruction code-store pc number-of-arguments)
  "Expand into a call of the appropriate machine instruction.

  `pc` should be a safe place representing the program counter.

  `code-store` should be a safe place representing the instructions.

  "
  `(,instruction ,wam
    ,@(loop :for i :from 1 :to number-of-arguments
            :collect `(aref ,code-store (+ ,pc ,i)))))


(defun extract-things (wam addresses)
  "Extract the things at the given heap addresses.

  The things will be returned in the same order as the addresses were given.

  Unbound variables will be turned into uninterned symbols.  There will only be
  one such symbol for any specific unbound var, so if two addresses are
  (eventually) bound to the same unbound var, the symbols returned from this
  function will be `eql`.

  "
  (let ((unbound-vars (list)))
    (labels
        ((mark-unbound-var (address)
           (let ((symbol (make-symbol (format nil "var-~D" ; lol
                                              (length unbound-vars)))))
             (car (push (cons address symbol) unbound-vars))))
         (extract-var (address)
           (cdr (or (assoc address unbound-vars)
                    (mark-unbound-var address))))
         (recur (address)
           (let ((cell (wam-heap-cell wam (deref wam address))))
             (cond
               ((cell-null-p cell) "NULL?!")
               ((cell-reference-p cell) (extract-var (cell-value cell)))
               ((cell-structure-p cell) (recur (cell-value cell)))
               ((cell-functor-p cell)
                (destructuring-bind (functor . arity)
                    (wam-functor-lookup wam (cell-functor-index cell))
                  (if (zerop arity)
                    functor
                    (list* functor
                           (mapcar #'recur
                                   (range (+ address 1)
                                          (+ address arity 1)))))))
               (t (error "What to heck is this?"))))))
      (mapcar #'recur addresses))))

(defun extract-query-results (wam vars)
  (let* ((addresses (loop :for var :in vars
                          :for i :from 0
                          :collect (wam-stack-frame-arg wam i 0)))
         (results (extract-things wam addresses)))
    (pairlis vars results)))

(defun print-query-results (results)
  (loop :for (var . result) :in results :do
        (format t "~S = ~S~%" var result)))


(defun run (wam &optional (step nil))
  (with-slots (code program-counter fail) wam
    (macrolet ((instruction (inst args)
                 `(instruction-call wam ,inst code program-counter ,args)))
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
            ;; need to skip the PC increment for PROC/CALL/DEAL/DONE
            ;; TODO: this is ugly
            (+opcode-deallocate+
              (instruction %deallocate 0)
              (return-from op))
            (+opcode-proceed+
              (instruction %proceed 0)
              (return-from op))
            (+opcode-call+
              (instruction %call 1)
              (return-from op))
            (+opcode-done+
              (return-from run)))
          (incf program-counter (instruction-size opcode))
          (when (>= program-counter (fill-pointer code))
            (error "Fell off the end of the program code store!")))))
    (values)))

(defun run-query (wam term &optional (step nil))
  "Compile query `term` and run the instructions on the `wam`.

  Resets the heap, etc before running.

  When `step` is true, break into the debugger before calling the procedure and
  after each instruction.

  "
  (multiple-value-bind (code vars)
      (compile-query wam term)
    (wam-reset! wam)
    (wam-load-query-code! wam code)
    (setf (wam-program-counter wam) 0
          (wam-continuation-pointer wam) +code-sentinal+)
    (when step
      (format *debug-io* "Built query code:~%")
      (dump-code-store wam code))
    (run wam step)
    (if (wam-fail wam)
      (princ "No.")
      (progn
        (print-query-results (extract-query-results wam vars))
        (princ "Yes."))))
  (values))


