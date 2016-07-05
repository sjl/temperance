(in-package #:bones.wam)

;;;; Config
(defparameter *break-on-fail* nil)
(defparameter *step* nil)


;;;; Utilities
(defun* push-unbound-reference! ((wam wam))
  (:returns (values cell heap-index))
  "Push a new unbound reference cell onto the heap."
  (wam-heap-push! wam (make-cell-reference (wam-heap-pointer wam))))

(defun* push-new-structure! ((wam wam))
  (:returns (values cell heap-index))
  "Push a new structure cell onto the heap.

  The structure cell's value will point at the next address, so make sure you
  push something there too!

  "
  (wam-heap-push! wam (make-cell-structure (1+ (wam-heap-pointer wam)))))

(defun* push-new-list! ((wam wam))
  (:returns (values cell heap-index))
  "Push a new list cell onto the heap.

  The list cell's value will point at the next address, so make sure you push
  something there too!

  "
  (wam-heap-push! wam (make-cell-list (1+ (wam-heap-pointer wam)))))

(defun* push-new-functor! ((wam wam) (functor functor-index))
  (:returns (values cell heap-index))
  "Push a new functor cell onto the heap."
  (wam-heap-push! wam (make-cell-functor functor)))


(declaim (inline bound-reference-p
                 unbound-reference-p
                 matching-functor-p
                 functors-match-p
                 constants-match-p))
(defun* bound-reference-p ((wam wam) (address store-index))
  "Return whether the cell at `address` is a bound reference."
  (let ((cell (wam-store-cell wam address)))
    (and (cell-reference-p cell)
         (not (= (cell-value cell) address)))))

(defun* unbound-reference-p ((wam wam) (address store-index))
  "Return whether the cell at `address` is an unbound reference."
  (let ((cell (wam-store-cell wam address)))
    (and (cell-reference-p cell)
         (= (cell-value cell) address))))

(defun* matching-functor-p ((cell cell)
                            (functor functor-index))
  "Return whether `cell` is a functor cell containing `functor`."
  (and (cell-functor-p cell)
       (= (cell-value cell) functor)))

(defun* functors-match-p ((functor-cell-1 cell)
                          (functor-cell-2 cell))
  (:returns boolean)
  "Return whether the two functor cells represent the same functor."
  (= (cell-value functor-cell-1)
     (cell-value functor-cell-2)))

(defun* constants-match-p ((constant-cell-1 cell)
                           (constant-cell-2 cell))
  (:returns boolean)
  "Return whether the two constant cells represent the same functor."
  (= (cell-value constant-cell-1)
     (cell-value constant-cell-2)))


(defmacro with-cell ((address-symbol cell-symbol) wam target &body body)
  "Bind variables to the (dereferenced) contents of the cell

  `target` should be an address in the WAM store.

  `address-symbol` and `cell-symbol` will be bound to the final address/cell
  after dereferencing `target.`

  "
  (once-only (wam target)
    `(let* ((,address-symbol (deref ,wam ,target))
            (,cell-symbol (wam-store-cell ,wam ,address-symbol)))
      ,@body)))


;;;; "Ancillary" Functions
(defun* backtrack! ((wam wam))
  (:returns :void)
  "Backtrack after a failure.

  If `*break-on-fail*` is true, the debugger will be invoked.

  "
  (when *break-on-fail*
    (break "Backtracked."))
  (if (wam-backtrack-pointer-unset-p wam)
    (setf (wam-fail wam) t)
    (setf (wam-program-counter wam) (wam-stack-choice-bp wam)
          (wam-backtracked wam) t))
  (values))

(defun* trail! ((wam wam) (address store-index))
  (:returns :void)
  "Push the given address onto the trail (but only if necessary)."
  (when (< address (wam-heap-backtrack-pointer wam))
    (wam-trail-push! wam address))
  (values))

(defun* unbind! ((wam wam) (address store-index))
  (:returns :void)
  "Unbind the reference cell at `address`.

  No error checking is done, so please don't try to unbind something that's not
  a reference cell.

  "
  (setf (wam-store-cell wam address)
        (make-cell-reference address))
  (values))

(defun* unwind-trail! ((wam wam)
                       (trail-start trail-index)
                       (trail-end trail-index))
  (:returns :void)
  "Unbind all the things in the given range of the trail."
  ;; TODO: seriously can't we just pop back to a certain place?
  (loop :for i :from trail-start :below trail-end :do
        (unbind! wam (wam-trail-value wam i)))
  (values))

(defun* tidy-trail! ((wam wam))
  (with-accessors ((tr wam-trail-pointer)
                   (h wam-heap-pointer)
                   (hb wam-heap-backtrack-pointer)
                   (b wam-backtrack-pointer))
      wam
    (loop
      ;; The book is, yet again, fucked.  It just sets `i` to be the trail
      ;; pointer from the choice point frame.  But what if we just popped off
      ;; the last choice point?  If that's the case we need to look over the
      ;; entire trail.
      :with i = (if (wam-backtrack-pointer-unset-p wam b)
                  0
                  (wam-stack-choice-tr wam))
      :for target = (wam-trail-value wam i)
      :while (< i tr) :do
      (if (or (< target hb)
              (and (< h target)
                   (< target b)))
        (incf i)
        (progn
          (setf (wam-trail-value wam i)
                (wam-trail-value wam (1- tr)))
          (decf tr))))))

(defun* deref ((wam wam) (address store-index))
  (:returns store-index)
  "Dereference the address in the WAM store to its eventual destination.

  If the address is a variable that's bound to something, that something will be
  looked up (recursively) and the address of whatever it's ultimately bound to
  will be returned.

  "
  (if (bound-reference-p wam address)
    (deref wam (cell-value (wam-store-cell wam address)))
    address))

(defun* bind! ((wam wam) (address-1 store-index) (address-2 store-index))
  (:returns :void)
  "Bind the unbound reference cell to the other.

  `bind!` takes two addresses as arguments.  You are expected to have `deref`ed
  previously to obtain these addresses, so neither of them should ever refer to
  a bound reference.

  At least one of the arguments *must* refer to an unbound reference cell.  This
  unbound reference will be bound to point at the other address.

  If *both* addresses refer to unbound references, the direction of the binding
  is chosen arbitrarily.

  "
  ;; In case it's not absolutely clear from the book: binding has to actually
  ;; COPY the source cell into the destination.
  ;;
  ;; It can't just update the cell value of the destination REF, because if
  ;; you're binding a REF on the heap to something in a register then doing so
  ;; would end up with a REF to a register address.  This would be bad because
  ;; that register would probably get clobbered later, and the REF would now be
  ;; pointing to garbage.
  (let ((cell-1 (wam-store-cell wam address-1))
        (cell-2 (wam-store-cell wam address-2)))
    (cond
      ;; Bind (a1 <- a2) if:
      ;; 
      ;; * A1 is a REF and A2 is something else, or...
      ;; * They're both REFs but A2 has a lower address than A1.
      ((and (cell-reference-p cell-1)
            (or (not (cell-reference-p cell-2))
                (< address-2 address-1)))
       (setf (wam-store-cell wam address-1) cell-2)
       (trail! wam address-1))
      ;; Bind (a2 <- a1) if A2 is a REF and A1 is something else.
      ((cell-reference-p cell-2)
       (setf (wam-store-cell wam address-2) cell-1)
       (trail! wam address-2))
      ;; wut
      (t
       (error "At least one cell must be an unbound reference when binding."))))
  (values))

(defun* unify! ((wam wam) (a1 store-index) (a2 store-index))
  (wam-unification-stack-push! wam a1)
  (wam-unification-stack-push! wam a2)
  (setf (wam-fail wam) nil)
  ;; TODO: refactor this horror show.
  (until (or (wam-fail wam)
             (wam-unification-stack-empty-p wam))
    (let ((d1 (deref wam (wam-unification-stack-pop! wam)))
          (d2 (deref wam (wam-unification-stack-pop! wam))))
      (when (not (= d1 d2))
        (let ((cell-1 (wam-store-cell wam d1))
              (cell-2 (wam-store-cell wam d2)))
          (cond
            ;; If at least one is a reference, bind them.
            ;;
            ;; We know that any references we see here will be unbound, because
            ;; we deref'ed them above.
            ((or (cell-reference-p cell-1) (cell-reference-p cell-2))
             (bind! wam d1 d2))

            ;; Otherwise if they're both constants, make sure they match.
            ((and (cell-constant-p cell-1) (cell-constant-p cell-2))
             (when (not (constants-match-p cell-1 cell-2))
               (backtrack! wam)))

            ;; Otherwise if they're both lists, make sure their contents match.
            ((and (cell-list-p cell-1) (cell-list-p cell-2))
             (wam-unification-stack-push! wam (cell-value cell-1))
             (wam-unification-stack-push! wam (cell-value cell-2))
             (wam-unification-stack-push! wam (1+ (cell-value cell-1)))
             (wam-unification-stack-push! wam (1+ (cell-value cell-2))))

            ;; Otherwise if they're both structure cells, make sure they match
            ;; and then schedule their subterms to be unified.
            ((and (cell-structure-p cell-1) (cell-structure-p cell-2))
             (let* ((structure-1-addr (cell-value cell-1)) ; find where they
                    (structure-2-addr (cell-value cell-2)) ; start on the heap
                    (functor-1 (wam-store-cell wam structure-1-addr)) ; grab the
                    (functor-2 (wam-store-cell wam structure-2-addr))) ; functors
               (if (functors-match-p functor-1 functor-2)
                 ;; If the functors match, push their pairs of arguments onto
                 ;; the stack to be unified.
                 (loop :with arity = (wam-functor-arity wam (cell-value functor-1))
                       :for i :from 1 :to arity :do
                       (wam-unification-stack-push! wam (+ structure-1-addr i))
                       (wam-unification-stack-push! wam (+ structure-2-addr i)))
                 ;; Otherwise we're hosed.
                 (backtrack! wam))))

            ;; Otherwise we're looking at two different kinds of cells, and are
            ;; just totally hosed.  Backtrack.
            (t (backtrack! wam))))))))


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
(define-instruction %put-structure
    ((wam wam)
     (functor functor-index)
     (register register-index))
  (setf (wam-local-register wam register)
        (make-cell-structure
          (nth-value 1 (push-new-functor! wam functor)))))

(define-instruction %put-list
    ((wam wam)
     (register register-index))
  (setf (wam-local-register wam register)
        (make-cell-list (wam-heap-pointer wam))))

(define-instructions (%set-variable-local %set-variable-stack)
    ((wam wam)
     (register register-index))
  (setf (%wam-register% wam register)
        (push-unbound-reference! wam)))

(define-instructions (%set-value-local %set-value-stack)
    ((wam wam)
     (register register-index))
  (wam-heap-push! wam (%wam-register% wam register)))

(define-instruction %set-void ((wam wam) (n arity))
  (repeat n
    (push-unbound-reference! wam)))

(define-instructions (%put-variable-local %put-variable-stack)
    ((wam wam)
     (register register-index)
     (argument register-index))
  (let ((new-reference (push-unbound-reference! wam)))
    (setf (%wam-register% wam register) new-reference
          (wam-local-register wam argument) new-reference)))

(define-instructions (%put-value-local %put-value-stack)
    ((wam wam)
     (register register-index)
     (argument register-index))
  (setf (wam-local-register wam argument)
        (%wam-register% wam register)))


;;;; Program Instructions
(define-instruction %get-structure ((wam wam)
                                    (functor functor-index)
                                    (register register-index))
  (with-accessors ((mode wam-mode) (s wam-subterm)) wam
    (with-cell (addr cell) wam register
      (cond
        ;; If the register points at a reference cell, we push two new cells
        ;; onto the heap:
        ;;
        ;;     |   N | STR | N+1 |
        ;;     | N+1 | FUN | f/n |
        ;;     |     |     |     | <- S
        ;;
        ;; Then we bind this reference cell to point at the new structure, set
        ;; the S register to point beneath it and flip over to write mode.
        ;;
        ;; It seems a bit confusing that we don't push the rest of the structure
        ;; stuff on the heap after it too.  But that's going to happen in the
        ;; next few instructions (which will be unify-*'s, executed in write
        ;; mode).
        ((cell-reference-p cell)
         (let ((structure-address (nth-value 1 (push-new-structure! wam)))
               (functor-address (nth-value 1 (push-new-functor! wam functor))))
           (bind! wam addr structure-address)
           (setf mode :write
                 s (1+ functor-address))))

        ;; If the register points at a structure cell, then we look at where
        ;; that cell points (which will be the functor cell for the structure):
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
        ;; garbage.  But that's okay, because we know the next thing in the
        ;; stream of instructions will be another get-structure and we'll just
        ;; blow away the S register there.
        ((cell-structure-p cell)
         (let* ((functor-address (cell-value cell))
                (functor-cell (wam-heap-cell wam functor-address)))
           (if (matching-functor-p functor-cell functor)
             (setf mode :read
                   s (1+ functor-address))
             (backtrack! wam))))

        (t (backtrack! wam))))))

(define-instruction %get-list ((wam wam)
                               (register register-index))
  (with-cell (addr cell) wam register
    (cond
      ;; If the register points at a reference (unbound, because we deref'ed) we
      ;; bind it to a list and flip into write mode to write the upcoming two
      ;; things as its contents.
      ((cell-reference-p cell)
       (bind! wam addr (nth-value 1 (push-new-list! wam)))
       (setf (wam-mode wam) :write))

      ;; If this is a list, we need to unify its subterms.
      ((cell-list-p cell)
       (setf (wam-mode wam) :read
             (wam-subterm wam) (cell-value cell)))

      (t (backtrack! wam)))))

(define-instructions (%unify-variable-local %unify-variable-stack)
    ((wam wam)
     (register register-index))
  (setf (%wam-register% wam register)
        (ecase (wam-mode wam)
          (:read (wam-heap-cell wam (wam-subterm wam)))
          (:write (push-unbound-reference! wam))))
  (incf (wam-subterm wam)))

(define-instructions (%unify-value-local %unify-value-stack)
    ((wam wam)
     (register register-index))
  (ecase (wam-mode wam)
    (:read (unify! wam register (wam-subterm wam)))
    (:write (wam-heap-push! wam (%wam-register% wam register))))
  (incf (wam-subterm wam)))

(define-instruction %unify-void ((wam wam) (n arity))
  (ecase (wam-mode wam)
    (:read (incf (wam-subterm wam) n))
    (:write (repeat n
              (push-unbound-reference! wam)))))

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
  (unify! wam register argument))


;;;; Control Instructions
(define-instruction %call ((wam wam) (functor functor-index)
                           &optional (program-counter-increment
                                       (instruction-size +opcode-call+)))
  (let ((target (wam-code-label wam functor)))
    (if target
      (setf (wam-continuation-pointer wam) ; CP <- next instruction
            (+ (wam-program-counter wam) program-counter-increment)

            (wam-number-of-arguments wam) ; set NARGS
            (wam-functor-arity wam functor)

            (wam-cut-pointer wam) ; set B0 in case we have a cut
            (wam-backtrack-pointer wam)

            (wam-program-counter wam) ; jump
            target)
      ;; Trying to call an unknown procedure.
      (backtrack! wam))))

(define-instruction %dynamic-call ((wam wam))
  ;; It's assumed that whatever we want to dynamically call has been put in
  ;; argument register zero.
  (with-cell (addr cell) wam 0 ; A_0
    (cond
      ((cell-structure-p cell)
       (with-cell (functor-address functor-cell) wam (cell-value cell)
         (let ((functor (cell-value functor-cell)))
           ;; If we have a non-zero-arity structure, we need to set up the
           ;; argument registers before we call it.  Luckily all the arguments
           ;; conveniently live contiguously right after the functor cell.
           (loop :with arity = (wam-functor-arity wam functor)
                 :for argument-register :from 0 :below arity
                 :for argument-address :from (1+ functor-address)
                 :do (setf (wam-local-register wam argument-register)
                           (wam-heap-cell wam argument-address)))
           (%call wam functor (instruction-size +opcode-dynamic-call+)))))
      ((cell-constant-p cell)
       ;; Zero-arity functors don't need to set up anything at all -- we can
       ;; just call them immediately.
       (%call wam (cell-value cell) (instruction-size +opcode-dynamic-call+)))
      ((cell-reference-p cell)
       ;; It's okay to do (call :var), but :var has to be bound by the time you
       ;; actually reach it at runtime.
       (error "Cannot dynamically call an unbound variable."))
      (t ; You can't (call) anything else.
       (error "Cannot dynamically call something other than a structure.")))))

(define-instruction %proceed ((wam wam))
  (setf (wam-program-counter wam) ; P <- CP
        (wam-continuation-pointer wam)))

(define-instruction %allocate ((wam wam) (n stack-frame-argcount))
  (let ((old-e (wam-environment-pointer wam))
        (new-e (wam-stack-top wam)))
    (wam-stack-ensure-size wam (+ new-e 4 n))
    (setf (wam-stack-word wam new-e) old-e ; CE
          (wam-stack-word wam (+ new-e 1)) (wam-continuation-pointer wam) ; CP
          (wam-stack-word wam (+ new-e 2)) (wam-cut-pointer wam) ; B0
          (wam-stack-word wam (+ new-e 3)) n ; N
          (wam-environment-pointer wam) new-e))) ; E <- new-e

(define-instruction %deallocate ((wam wam))
  (setf (wam-program-counter wam) (wam-stack-frame-cp wam)
        (wam-environment-pointer wam) (wam-stack-frame-ce wam)
        (wam-cut-pointer wam) (wam-stack-frame-cut wam)))


;;;; Choice Instructions
(defun* reset-choice-point! ((wam wam)
                             (b backtrack-pointer))
  (setf (wam-backtrack-pointer wam) b

        ;; The book is wrong here: when resetting HB we use the NEW value of B,
        ;; so the heap backtrack pointer gets set to the heap pointer saved in
        ;; the PREVIOUS choice point.  Thanks to the errata at
        ;; https://github.com/a-yiorgos/wambook/blob/master/wamerratum.txt for
        ;; pointing this out.
        ;;
        ;; ... well, almost.  The errata is also wrong here.  If we're popping
        ;; the FIRST choice point, then just using the HB from the "previous
        ;; choice point" is going to give us garbage, so we should check for
        ;; that edge case too.  Please kill me.
        (wam-heap-backtrack-pointer wam)
        (if (wam-backtrack-pointer-unset-p wam b)
          +heap-start+
          (wam-stack-choice-h wam b))))

(define-instruction %try ((wam wam) (next-clause code-index))
  (let ((new-b (wam-stack-top wam))
        (nargs (wam-number-of-arguments wam)))
    (wam-stack-ensure-size wam (+ new-b 7 nargs))
    (setf (wam-stack-word wam new-b) nargs ; N
          (wam-stack-word wam (+ new-b 1)) (wam-environment-pointer wam) ; CE
          (wam-stack-word wam (+ new-b 2)) (wam-continuation-pointer wam) ; CP
          (wam-stack-word wam (+ new-b 3)) (wam-backtrack-pointer wam) ; CB
          (wam-stack-word wam (+ new-b 4)) next-clause ; BP
          (wam-stack-word wam (+ new-b 5)) (wam-trail-pointer wam) ; TR
          (wam-stack-word wam (+ new-b 6)) (wam-heap-pointer wam) ; H
          (wam-heap-backtrack-pointer wam) (wam-heap-pointer wam) ; HB
          (wam-backtrack-pointer wam) new-b) ; B
    (loop :for i :from 0 :below nargs :do ; A_i
          (setf (wam-stack-choice-arg wam i new-b)
                (wam-local-register wam i)))))

(define-instruction %retry ((wam wam) (next-clause code-index))
  (let ((b (wam-backtrack-pointer wam)))
    ;; Restore argument registers
    (loop :for i :from 0 :below (wam-stack-choice-n wam b) :do
          (setf (wam-local-register wam i)
                (wam-stack-choice-arg wam i b)))
    (unwind-trail! wam (wam-stack-choice-tr wam b) (wam-trail-pointer wam))
    (setf (wam-environment-pointer wam) (wam-stack-choice-ce wam b)
          (wam-continuation-pointer wam) (wam-stack-choice-cp wam b)
          ;; overwrite the next clause address in the choice point
          (wam-stack-word wam (+ b 4)) next-clause
          (wam-trail-pointer wam) (wam-stack-choice-tr wam b)
          (wam-heap-pointer wam) (wam-stack-choice-h wam b)
          (wam-heap-backtrack-pointer wam) (wam-heap-pointer wam))))

(define-instruction %trust ((wam wam))
  (let* ((b (wam-backtrack-pointer wam))
         (old-b (wam-stack-choice-cb wam b)))
    ;; Restore argument registers
    (loop :for i :from 0 :below (wam-stack-choice-n wam b) :do
          (setf (wam-local-register wam i)
                (wam-stack-choice-arg wam i b)))
    (unwind-trail! wam (wam-stack-choice-tr wam b) (wam-trail-pointer wam))
    (setf (wam-environment-pointer wam) (wam-stack-choice-ce wam b)
          (wam-continuation-pointer wam) (wam-stack-choice-cp wam b)
          (wam-trail-pointer wam) (wam-stack-choice-tr wam b)
          (wam-heap-pointer wam) (wam-stack-choice-h wam b))
    (reset-choice-point! wam old-b)))

(define-instruction %cut ((wam wam))
  (let ((current-choice-point (wam-backtrack-pointer wam))
        (previous-choice-point (wam-stack-frame-cut wam)))
    (when (< previous-choice-point current-choice-point)
      (reset-choice-point! wam previous-choice-point)
      (tidy-trail! wam))))


;;;; Constant Instructions
(defun* %%match-constant ((wam wam)
                          (constant functor-index)
                          (address store-index))
  (with-cell (addr cell) wam address
    (cond
      ((cell-reference-p cell)
       (setf (wam-store-cell wam addr)
             (make-cell-constant constant))
       (trail! wam addr))

      ((cell-constant-p cell)
       (when (not (= constant (cell-value cell)))
         (backtrack! wam)))

      (t
       (backtrack! wam)))))

(define-instruction %put-constant ((wam wam)
                                   (constant functor-index)
                                   (register register-index))
  (setf (wam-local-register wam register)
        (make-cell-constant constant)))

(define-instruction %get-constant ((wam wam)
                                   (constant functor-index)
                                   (register register-index))
  (%%match-constant wam constant register))

(define-instruction %set-constant ((wam wam)
                                   (constant functor-index))
  (wam-heap-push! wam (make-cell-constant constant)))

(define-instruction %unify-constant ((wam wam)
                                     (constant functor-index))
  (ecase (wam-mode wam)
    (:read (%%match-constant wam constant (wam-subterm wam)))
    (:write (wam-heap-push! wam (make-cell-constant constant)))))


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
  "Extract the things at the given store addresses.

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
           (let ((cell (wam-store-cell wam (deref wam address))))
             (cond
               ((cell-null-p cell) "NULL?!")
               ((cell-reference-p cell) (extract-var (cell-value cell)))
               ((cell-structure-p cell) (recur (cell-value cell)))
               ((cell-list-p cell) (cons (recur (cell-value cell))
                                         (recur (1+ (cell-value cell)))))
               ((cell-constant-p cell)
                (wam-functor-symbol wam (cell-value cell)))
               ((cell-functor-p cell)
                (destructuring-bind (functor . arity)
                    (wam-functor-lookup wam (cell-value cell))
                  (list* functor
                         (loop :for addr
                               :from (+ address 1) :below (+ address arity 1)
                               :collect (recur addr)))))
               (t (error "What to heck is this?"))))))
      (mapcar #'recur addresses))))

(defun extract-query-results (wam vars)
  (let* ((addresses (loop :for var :in vars
                          ;; TODO: make this suck less
                          :for i :from (+ (wam-environment-pointer wam) 4)
                          :collect i))
         (results (extract-things wam addresses)))
    (weave vars results)))


(defun run (wam done-thunk)
  (with-accessors ((pc wam-program-counter)) wam
    (let ((code (wam-code wam)))
      (macrolet ((instruction (inst args)
                   `(instruction-call wam ,inst code pc ,args)))
        (loop
          :with increment-pc = t
          :while (and (not (wam-fail wam)) ; failure
                      (not (= pc +code-sentinel+))) ; finished
          :for opcode = (aref code pc)
          :do
          (block op
            (when *step*
              (dump) ; todo: make this saner
              (break "About to execute instruction at ~4,'0X" pc))
            (eswitch (opcode)
              ;; Query
              (+opcode-put-structure+        (instruction %put-structure 2))
              (+opcode-set-variable-local+   (instruction %set-variable-local 1))
              (+opcode-set-variable-stack+   (instruction %set-variable-stack 1))
              (+opcode-set-value-local+      (instruction %set-value-local 1))
              (+opcode-set-value-stack+      (instruction %set-value-stack 1))
              (+opcode-set-void+             (instruction %set-void 1))
              (+opcode-put-variable-local+   (instruction %put-variable-local 2))
              (+opcode-put-variable-stack+   (instruction %put-variable-stack 2))
              (+opcode-put-value-local+      (instruction %put-value-local 2))
              (+opcode-put-value-stack+      (instruction %put-value-stack 2))
              ;; Program
              (+opcode-get-structure+        (instruction %get-structure 2))
              (+opcode-unify-variable-local+ (instruction %unify-variable-local 1))
              (+opcode-unify-variable-stack+ (instruction %unify-variable-stack 1))
              (+opcode-unify-value-local+    (instruction %unify-value-local 1))
              (+opcode-unify-value-stack+    (instruction %unify-value-stack 1))
              (+opcode-unify-void+           (instruction %unify-void 1))
              (+opcode-get-variable-local+   (instruction %get-variable-local 2))
              (+opcode-get-variable-stack+   (instruction %get-variable-stack 2))
              (+opcode-get-value-local+      (instruction %get-value-local 2))
              (+opcode-get-value-stack+      (instruction %get-value-stack 2))
              ;; Constant
              (+opcode-put-constant+         (instruction %put-constant 2))
              (+opcode-get-constant+         (instruction %get-constant 2))
              (+opcode-set-constant+         (instruction %set-constant 1))
              (+opcode-unify-constant+       (instruction %unify-constant 1))
              ;; List
              (+opcode-put-list+             (instruction %put-list 1))
              (+opcode-get-list+             (instruction %get-list 1))
              ;; Choice
              (+opcode-try+                  (instruction %try 1))
              (+opcode-retry+                (instruction %retry 1))
              (+opcode-trust+                (instruction %trust 0))
              (+opcode-cut+                  (instruction %cut 0))
              ;; Control
              (+opcode-allocate+             (instruction %allocate 1))
              ;; need to skip the PC increment for PROC/CALL/DEAL/DONE
              ;; TODO: this is still ugly
              (+opcode-deallocate+
                (instruction %deallocate 0)
                (setf increment-pc nil))
              (+opcode-proceed+
                (instruction %proceed 0)
                (setf increment-pc nil))
              (+opcode-call+
                (instruction %call 1)
                (setf increment-pc nil))
              (+opcode-dynamic-call+
                (instruction %dynamic-call 0)
                (setf increment-pc nil))
              (+opcode-done+
                (if (funcall done-thunk)
                  (return-from run)
                  (backtrack! wam))))
            ;; Only increment the PC when we didn't backtrack.
            ;;
            ;; If we backtracked, the PC will have been filled in from the
            ;; choice point.
            (when (and increment-pc (not (wam-backtracked wam)))
              (incf pc (instruction-size opcode)))
            (setf (wam-backtracked wam) nil
                  increment-pc t)
            (when (>= pc (fill-pointer code))
              (error "Fell off the end of the program code store."))))))
    (values)))

(defun run-query (wam term
                  &key
                  (result-function (lambda (results) (declare (ignore results))))
                  (status-function (lambda (failp) (declare (ignore failp)))))
  "Compile query `term` and run the instructions on the `wam`.

  Resets the heap, etc before running.

  When `*step*` is true, break into the debugger before calling the procedure and
  after each instruction.

  "
  (multiple-value-bind (code vars)
      (compile-query wam term)
    (wam-reset! wam)
    (wam-load-query-code! wam code)
    (setf (wam-program-counter wam) 0
          (wam-continuation-pointer wam) +code-sentinel+)
    (when *step*
      (format *debug-io* "Built query code:~%")
      (dump-code-store wam code))
    (run wam (lambda ()
               (funcall result-function
                        (extract-query-results wam vars))))
    (when status-function
      (funcall status-function (wam-fail wam))))
  (values))


