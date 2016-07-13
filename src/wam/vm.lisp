(in-package #:bones.wam)

;;;; Config
(defvar *step* nil)


;;;; Utilities
(declaim (inline functors-match-p
                 constants-match-p))


(defun* push-unbound-reference! ((wam wam))
  (:returns heap-index)
  "Push a new unbound reference cell onto the heap, returning its address."
  (wam-heap-push! wam +cell-type-reference+ (wam-heap-pointer wam)))

(defun* push-new-structure! ((wam wam))
  (:returns heap-index)
  "Push a new structure cell onto the heap, returning its address.

  The structure cell's value will point at the next address, so make sure you
  push something there too!

  "
  (wam-heap-push! wam +cell-type-structure+ (1+ (wam-heap-pointer wam))))

(defun* push-new-list! ((wam wam))
  (:returns heap-index)
  "Push a new list cell onto the heap, returning its address.

  The list cell's value will point at the next address, so make sure you push
  something there too!

  "
  (wam-heap-push! wam +cell-type-list+ (1+ (wam-heap-pointer wam))))

(defun* push-new-functor! ((wam wam) (functor functor-index))
  (:returns heap-index)
  "Push a new functor cell onto the heap, returning its address."
  (wam-heap-push! wam +cell-type-functor+ functor))

(defun* push-new-constant! ((wam wam) (constant functor-index))
  (:returns heap-index)
  "Push a new constant cell onto the heap, returning its address."
  (wam-heap-push! wam +cell-type-constant+ constant))


(defun* functors-match-p ((f1 functor-index)
                          (f2 functor-index))
  (:returns boolean)
  "Return whether the two functor cell values represent the same functor."
  (= f1 f2))

(defun* constants-match-p ((c1 functor-index)
                           (c2 functor-index))
  (:returns boolean)
  "Return whether the two constant cells represent the same functor."
  (= c1 c2))


;;;; "Ancillary" Functions
(declaim (inline deref unbind! trail!))


(defun* backtrack! ((wam wam))
  "Backtrack after a failure."
  (if (wam-backtrack-pointer-unset-p wam)
    (setf (wam-fail wam) t)
    (setf (wam-program-counter wam) (wam-stack-choice-bp wam)
          (wam-cut-pointer wam) (wam-stack-choice-cc wam)
          (wam-backtracked wam) t)))

(defun* trail! ((wam wam) (address store-index))
  "Push the given address onto the trail (but only if necessary)."
  (when (< address (wam-heap-backtrack-pointer wam))
    (wam-trail-push! wam address)))

(defun* unbind! ((wam wam) (address store-index))
  "Unbind the reference cell at `address`.

  No error checking is done, so please don't try to unbind something that's not
  (originally) a reference cell.

  "
  (wam-set-store-cell! wam address +cell-type-reference+ address))

(defun* unwind-trail! ((wam wam)
                       (trail-start trail-index)
                       (trail-end trail-index))
  "Unbind all the things in the given range of the trail."
  ;; TODO: seriously can't we just pop back to a certain place?
  (loop :for i :from trail-start :below trail-end :do
        (unbind! wam (wam-trail-value wam i))))

(defun* tidy-trail! ((wam wam))
  (with-accessors ((tr wam-trail-pointer)
                   (h wam-heap-pointer)
                   (hb wam-heap-backtrack-pointer)
                   (b wam-backtrack-pointer)) wam
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
  ;; SBCL won't inline recursive functions :(
  (loop
    (cell-typecase (wam address)
      ((:reference ref) (if (= address ref)
                          (return address) ; unbound ref
                          (setf address ref))) ; bound ref
      (t (return address))))) ; non-ref

(defun* bind! ((wam wam) (address-1 store-index) (address-2 store-index))
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
  (cond
    ;; Bind (a1 <- a2) if:
    ;;
    ;; * A1 is a REF and A2 is something else, or...
    ;; * They're both REFs but A2 has a lower address than A1.
    ((and (cell-type-p (wam address-1) :reference)
          (or (not (cell-type-p (wam address-2) :reference))
              (< address-2 address-1)))
     (wam-copy-store-cell! wam address-1 address-2)
     (trail! wam address-1))

    ;; Bind (a2 <- a1) if A2 is a REF and A1 is something else.
    ((cell-type-p (wam address-2) :reference)
     (wam-copy-store-cell! wam address-2 address-1)
     (trail! wam address-2))

    ;; wut
    (t (error "At least one cell must be an unbound reference when binding."))))

(defun* unify! ((wam wam) (a1 store-index) (a2 store-index))
  (wam-unification-stack-push! wam a1)
  (wam-unification-stack-push! wam a2)
  (setf (wam-fail wam) nil)
  ;; TODO: refactor this horror show.
  (until (or (wam-fail wam)
             (wam-unification-stack-empty-p wam))
    (let* ((d1 (deref wam (wam-unification-stack-pop! wam)))
           (d2 (deref wam (wam-unification-stack-pop! wam)))
           (t1 (wam-store-type wam d1))
           (t2 (wam-store-type wam d2)))
      (when (not (= d1 d2))
        (cond
          ;; If at least one is a reference, bind them.
          ;;
          ;; We know that any references we see here will be unbound because
          ;; we deref'ed them above.
          ((or (cell-type= t1 :reference)
               (cell-type= t2 :reference))
           (bind! wam d1 d2))

          ;; Otherwise if they're both constants, make sure they match.
          ((and (cell-type= t1 :constant)
                (cell-type= t2 :constant))
           (let ((c1 (wam-store-value wam d1))
                 (c2 (wam-store-value wam d2)))
             (when (not (constants-match-p c1 c2))
               (backtrack! wam))))

          ;; Otherwise if they're both lists, unify their contents.
          ((and (cell-type= t1 :list)
                (cell-type= t2 :list))
           (wam-unification-stack-push! wam (wam-store-value wam d1))
           (wam-unification-stack-push! wam (wam-store-value wam d2))
           (wam-unification-stack-push! wam (1+ (wam-store-value wam d1)))
           (wam-unification-stack-push! wam (1+ (wam-store-value wam d2))))

          ;; Otherwise if they're both structures, make sure they match and
          ;; then schedule their subterms to be unified.
          ((and (cell-type= t1 :structure)
                (cell-type= t2 :structure))
           (let* ((s1 (wam-store-value wam d1)) ; find where they
                  (s2 (wam-store-value wam d2)) ; start on the heap
                  (f1 (wam-store-value wam s1)) ; grab the
                  (f2 (wam-store-value wam s2))) ; functors
             (if (functors-match-p f1 f2)
               ;; If the functors match, push their pairs of arguments onto
               ;; the stack to be unified.
               (loop :with arity = (wam-functor-arity wam f1)
                     :for i :from 1 :to arity :do
                     (wam-unification-stack-push! wam (+ s1 i))
                     (wam-unification-stack-push! wam (+ s2 i)))
               ;; Otherwise we're hosed.
               (backtrack! wam))))

          ;; Otherwise we're looking at two different kinds of cells, and are
          ;; just totally hosed.  Backtrack.
          (t (backtrack! wam)))))))


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
;;; To make the process of defining these two "variants" less excruciating we
;;; have these two macros.  `define-instruction` (singular) is just a little
;;; sugar around `defun*`, for those instructions that don't deal with
;;; arguments.
;;;
;;; `define-instructions` (plural) is the awful one.  You pass it a pair of
;;; symbols for the two variant names.  Two functions will be defined, both with
;;; the same body, with a few symbols macroletted to the appropriate access
;;; code.
;;;
;;; So in the body, instead of using:
;;;
;;;     (wam-set-{local/stack}-register wam reg type value)
;;;
;;; you use:
;;;
;;;     (%wam-set-register% wam reg type value)
;;;
;;; and it'll do the right thing.

(defmacro define-instruction
    ((name &optional should-inline) lambda-list &body body)
  "Define an instruction function.

  This is just sugar over `defun*`.

  "
  `(progn
    (declaim (,(if should-inline 'inline 'notinline) ,name))
    (defun* ,name ,lambda-list
      ,@body)))

(defmacro define-instructions
    ((local-name stack-name &optional should-inline) lambda-list &body body)
  "Define a local/stack pair of instructions."
  `(progn
    (macrolet ((%wam-register% (wam register)
                 `(wam-local-register-address ,wam ,register))
               (%wam-register-type% (wam register)
                 `(wam-local-register-type ,wam ,register))
               (%wam-register-value% (wam register)
                 `(wam-local-register-value ,wam ,register))
               (%wam-set-register% (wam register type value)
                 `(wam-set-local-register! ,wam ,register ,type ,value))
               (%wam-copy-to-register% (wam register source)
                 `(wam-copy-to-local-register! ,wam ,register ,source)))
      (define-instruction (,local-name ,should-inline) ,lambda-list
        ,@body))
    (macrolet ((%wam-register% (wam register)
                 `(wam-stack-register-address ,wam ,register))
               (%wam-register-type% (wam register)
                 `(wam-stack-register-type ,wam ,register))
               (%wam-register-value% (wam register)
                 `(wam-stack-register-value ,wam ,register))
               (%wam-set-register% (wam register type value)
                 `(wam-set-stack-register! ,wam ,register ,type ,value))
               (%wam-copy-to-register% (wam register source)
                 `(wam-copy-to-stack-register! ,wam ,register ,source)))
      (define-instruction (,stack-name ,should-inline) ,lambda-list
        ,@body))))


;;;; Query Instructions
(define-instruction (%put-structure)
    ((wam wam)
     (functor functor-index)
     (register register-index))
  (wam-set-local-register! wam register
                           +cell-type-structure+
                           (push-new-functor! wam functor))
  (setf (wam-mode wam) :write))

(define-instruction (%put-list)
    ((wam wam)
     (register register-index))
  (wam-set-local-register! wam register
                           +cell-type-list+
                           (wam-heap-pointer wam))
  (setf (wam-mode wam) :write))

(define-instructions (%put-variable-local %put-variable-stack)
    ((wam wam)
     (register register-index)
     (argument register-index))
  (let ((ref (push-unbound-reference! wam)))
    (%wam-copy-to-register% wam register ref)
    (wam-copy-to-local-register! wam argument ref)
    (setf (wam-mode wam) :write)))

(define-instructions (%put-value-local %put-value-stack)
    ((wam wam)
     (register register-index)
     (argument register-index))
  (wam-copy-to-local-register! wam argument (%wam-register% wam register))
  (setf (wam-mode wam) :write))


;;;; Program Instructions
(define-instruction (%get-structure) ((wam wam)
                                      (functor functor-index)
                                      (register register-index))
  (cell-typecase (wam (deref wam register) address)
    ;; If the register points at an unbound reference cell, we push two new
    ;; cells onto the heap:
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
    ;; next few instructions (which will be subterm-*'s, executed in write
    ;; mode).
    (:reference
     (let ((structure-address (push-new-structure! wam))
           (functor-address (push-new-functor! wam functor)))
       (bind! wam address structure-address)
       (setf (wam-mode wam) :write
             (wam-subterm wam) (1+ functor-address))))

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
    ((:structure functor-address)
     (cell-typecase (wam functor-address)
       ((:functor f)
        (if (functors-match-p functor f)
          (setf (wam-mode wam) :read
                (wam-subterm wam) (1+ functor-address))
          (backtrack! wam)))))

    ;; Otherwise we can't unify, so backtrack.
    (t (backtrack! wam))))

(define-instruction (%get-list) ((wam wam)
                                 (register register-index))
  (cell-typecase (wam (deref wam register) address)
    ;; If the register points at a reference (unbound, because we deref'ed) we
    ;; bind it to a list and flip into write mode to write the upcoming two
    ;; things as its contents.
    (:reference
     (bind! wam address (push-new-list! wam))
     (setf (wam-mode wam) :write))

    ;; If this is a list, we need to unify its subterms.
    ((:list contents)
     (setf (wam-mode wam) :read
           (wam-subterm wam) contents))

    ;; Otherwise we can't unify.
    (t (backtrack! wam))))

(define-instructions (%get-variable-local %get-variable-stack)
    ((wam wam)
     (register register-index)
     (argument register-index))
  (%wam-copy-to-register% wam register argument))

(define-instructions (%get-value-local %get-value-stack t)
    ((wam wam)
     (register register-index)
     (argument register-index))
  (unify! wam register argument))


;;;; Subterm Instructions
(define-instructions (%subterm-variable-local %subterm-variable-stack)
    ((wam wam)
     (register register-index))
  (%wam-copy-to-register% wam register
                          (ecase (wam-mode wam)
                            (:read (wam-subterm wam))
                            (:write (push-unbound-reference! wam))))
  (incf (wam-subterm wam)))

(define-instructions (%subterm-value-local %subterm-value-stack)
    ((wam wam)
     (register register-index))
  (ecase (wam-mode wam)
    (:read (unify! wam register (wam-subterm wam)))
    (:write (wam-heap-push! wam
                            (%wam-register-type% wam register)
                            (%wam-register-value% wam register))))
  (incf (wam-subterm wam)))

(define-instruction (%subterm-void) ((wam wam) (n arity))
  (ecase (wam-mode wam)
    (:read (incf (wam-subterm wam) n))
    (:write (repeat n
              (push-unbound-reference! wam)))))


;;;; Control Instructions
(declaim (inline %%procedure-call %%dynamic-procedure-call))


(defun* %%procedure-call ((wam wam)
                          (functor functor)
                          (program-counter-increment instruction-size)
                          (is-tail boolean))
  (let* ((findex (wam-ensure-functor-index wam functor)) ; todo unfuck this once we finish splitting
         (target (wam-code-label wam findex)))
    (if (not target)
      ;; Trying to call an unknown procedure.
      (backtrack! wam)
      (progn
        (when (not is-tail)
          (setf (wam-continuation-pointer wam) ; CP <- next instruction
                (+ (wam-program-counter wam) program-counter-increment)))
        (setf (wam-number-of-arguments wam) ; set NARGS
              (wam-functor-arity wam findex)

              (wam-cut-pointer wam) ; set B0 in case we have a cut
              (wam-backtrack-pointer wam)

              (wam-program-counter wam) ; jump
              target)))))

(defun* %%dynamic-procedure-call ((wam wam) (is-tail boolean))
  (flet ((%go (functor)
           (if is-tail
             (%%procedure-call
               wam functor (instruction-size +opcode-dynamic-jump+) t)
             (%%procedure-call
               wam functor (instruction-size +opcode-dynamic-call+) nil)))
         (load-arguments (n start-address)
           (loop :for arg :from 0 :below n
                 :for source :from start-address
                 :do (wam-copy-to-local-register! wam arg source))))
    (cell-typecase (wam (deref wam 0)) ; A_0
      ((:structure functor-address)
       ;; If we have a non-zero-arity structure, we need to set up the
       ;; argument registers before we call it.  Luckily all the arguments
       ;; conveniently live contiguously right after the functor cell.
       (cell-typecase (wam functor-address)
         ((:functor f)
          (load-arguments (wam-functor-arity wam f) (1+ functor-address))
          (%go (wam-functor-lookup wam f)))))
      ((:constant c)
       ;; Zero-arity functors don't need to set up anything at all -- we can
       ;; just call them immediately.
       (%go (wam-functor-lookup wam c)))
      (:reference
       ;; It's okay to do (call :var), but :var has to be bound by the time you
       ;; actually reach it at runtime.
       (error "Cannot dynamically call an unbound variable."))
      (t ; You can't call/1 anything else.
       (error "Cannot dynamically call something other than a structure.")))))


(define-instruction (%jump) ((wam wam) (functor functor))
  (%%procedure-call wam functor (instruction-size +opcode-jump+) t))

(define-instruction (%call) ((wam wam) (functor functor))
  (%%procedure-call wam functor (instruction-size +opcode-call+) nil))


(define-instruction (%dynamic-call) ((wam wam))
  (%%dynamic-procedure-call wam nil))

(define-instruction (%dynamic-jump) ((wam wam))
  (%%dynamic-procedure-call wam t))


(define-instruction (%proceed t) ((wam wam))
  (setf (wam-program-counter wam) ; P <- CP
        (wam-continuation-pointer wam)))

(define-instruction (%allocate) ((wam wam) (n stack-frame-argcount))
  (let ((old-e (wam-environment-pointer wam))
        (new-e (wam-stack-top wam)))
    (wam-stack-ensure-size wam (+ new-e 4 n))
    (setf (wam-stack-word wam new-e) old-e ; CE
          (wam-stack-word wam (+ new-e 1)) (wam-continuation-pointer wam) ; CP
          (wam-stack-word wam (+ new-e 2)) (wam-cut-pointer wam) ; B0
          (wam-stack-word wam (+ new-e 3)) n ; N
          (wam-environment-pointer wam) new-e))) ; E <- new-e

(define-instruction (%deallocate) ((wam wam))
  (setf (wam-continuation-pointer wam) (wam-stack-frame-cp wam)
        (wam-environment-pointer wam) (wam-stack-frame-ce wam)
        (wam-cut-pointer wam) (wam-stack-frame-cut wam)))


;;;; Choice Instructions
(declaim (inline reset-choice-point!))


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

(defun* restore-registers-from-choice-point! ((wam wam)
                                              (b backtrack-pointer))
  (loop :for register :from 0 :below (wam-stack-choice-n wam b)
        :for saved-register :from (wam-stack-choice-argument-address wam 0 b)
        :do (wam-copy-to-local-register! wam register saved-register)))


(define-instruction (%try) ((wam wam) (next-clause code-index))
  (let ((new-b (wam-stack-top wam))
        (nargs (wam-number-of-arguments wam)))
    (wam-stack-ensure-size wam (+ new-b 8 nargs))
    (setf (wam-stack-word wam new-b) nargs ; N
          (wam-stack-word wam (+ new-b 1)) (wam-environment-pointer wam) ; CE
          (wam-stack-word wam (+ new-b 2)) (wam-continuation-pointer wam) ; CP
          (wam-stack-word wam (+ new-b 3)) (wam-backtrack-pointer wam) ; CB
          (wam-stack-word wam (+ new-b 4)) next-clause ; BP
          (wam-stack-word wam (+ new-b 5)) (wam-trail-pointer wam) ; TR
          (wam-stack-word wam (+ new-b 6)) (wam-heap-pointer wam) ; H
          (wam-stack-word wam (+ new-b 7)) (wam-cut-pointer wam) ; CC
          (wam-heap-backtrack-pointer wam) (wam-heap-pointer wam) ; HB
          (wam-backtrack-pointer wam) new-b) ; B
    (loop :for i :from 0 :below nargs ; A_i
          :for n :from 0 :below nargs ; arg N in the choice point frame
          :do (wam-copy-to-stack-choice-argument! wam n i new-b))))

(define-instruction (%retry) ((wam wam) (next-clause code-index))
  (let ((b (wam-backtrack-pointer wam)))
    (restore-registers-from-choice-point! wam b)
    (unwind-trail! wam (wam-stack-choice-tr wam b) (wam-trail-pointer wam))
    (setf (wam-environment-pointer wam) (wam-stack-choice-ce wam b)
          (wam-continuation-pointer wam) (wam-stack-choice-cp wam b)
          ;; overwrite the next clause address in the choice point
          (wam-stack-word wam (+ b 4)) next-clause
          (wam-trail-pointer wam) (wam-stack-choice-tr wam b)
          (wam-heap-pointer wam) (wam-stack-choice-h wam b)
          (wam-heap-backtrack-pointer wam) (wam-heap-pointer wam))))

(define-instruction (%trust) ((wam wam))
  (let* ((b (wam-backtrack-pointer wam))
         (old-b (wam-stack-choice-cb wam b)))
    (restore-registers-from-choice-point! wam b)
    (unwind-trail! wam (wam-stack-choice-tr wam b) (wam-trail-pointer wam))
    (setf (wam-environment-pointer wam) (wam-stack-choice-ce wam b)
          (wam-continuation-pointer wam) (wam-stack-choice-cp wam b)
          (wam-trail-pointer wam) (wam-stack-choice-tr wam b)
          (wam-heap-pointer wam) (wam-stack-choice-h wam b))
    (reset-choice-point! wam old-b)))

(define-instruction (%cut) ((wam wam))
  (let ((current-choice-point (wam-backtrack-pointer wam))
        (previous-choice-point (wam-stack-frame-cut wam)))
    (when (< previous-choice-point current-choice-point)
      (reset-choice-point! wam previous-choice-point)
      (tidy-trail! wam))))


;;;; Constant Instructions
(declaim (inline %%match-constant))


(defun* %%match-constant
    ((wam wam)
     (constant functor-index)
     (address store-index))
  (cell-typecase (wam (deref wam address) address)
    (:reference
     (wam-set-store-cell! wam address +cell-type-constant+ constant)
     (trail! wam address))

    ((:constant c)
     (when (not (= constant c))
       (backtrack! wam)))

    (t (backtrack! wam))))


(define-instruction (%put-constant)
    ((wam wam)
     (constant functor-index)
     (register register-index))
  (wam-set-local-register! wam register +cell-type-constant+ constant)
  ; todo we can probably elide this because constants never have subterms...
  (setf (wam-mode wam) :write))

(define-instruction (%get-constant)
    ((wam wam)
     (constant functor-index)
     (register register-index))
  (%%match-constant wam constant register))

(define-instruction (%subterm-constant)
    ((wam wam)
     (constant functor-index))
  (ecase (wam-mode wam)
    (:read (%%match-constant wam constant (wam-subterm wam)))
    (:write (push-new-constant! wam constant)))
  (incf (wam-subterm wam)))


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
           (let ((symbol (make-symbol (format nil "?VAR-~D" ; lol
                                              (length unbound-vars)))))
             (car (push (cons address symbol) unbound-vars))))
         (extract-var (address)
           (cdr (or (assoc address unbound-vars)
                    (mark-unbound-var address))))
         (recur (address)
           (cell-typecase (wam (deref wam address) address)
             (:null "NULL?!")
             ((:reference r) (extract-var r))
             ((:structure s) (recur s))
             ((:list l) (cons (recur l) (recur (1+ l))))
             ((:constant c) (wam-functor-symbol wam c))
             ((:functor f)
              (destructuring-bind (functor . arity)
                  (wam-functor-lookup wam f)
                (list* functor
                       (loop :repeat arity
                             :for subterm :from (+ address 1)
                             :collect (recur subterm)))))
             (t (error "What to heck is this?")))))
      (mapcar #'recur addresses))))

(defun extract-query-results (wam vars)
  (let* ((addresses (loop :for var :in vars
                          ;; TODO: make this suck less
                          :for i :from (+ (wam-environment-pointer wam) 4)
                          :collect i))
         (results (extract-things wam addresses)))
    (weave vars results)))


(defun* run ((wam wam) (done-thunk function))
  (with-accessors ((pc wam-program-counter)) wam
    (let ((code (wam-code wam)))
      (macrolet ((instruction (inst args)
                   `(instruction-call wam ,inst code pc ,args)))
        (loop
          :with increment-pc = t
          :while (and (not (wam-fail wam)) ; failure
                      (not (= pc +code-sentinel+))) ; finished
          :for opcode = (aref code pc) ; todo switch this to wam-code-word...
          :do
          (progn
            (when *step*
              (dump) ; todo: make this saner
              (break "About to execute instruction at ~4,'0X" pc))
            (ecase opcode
              ;; Query
              (#.+opcode-put-structure+          (instruction %put-structure 2))
              (#.+opcode-put-variable-local+     (instruction %put-variable-local 2))
              (#.+opcode-put-variable-stack+     (instruction %put-variable-stack 2))
              (#.+opcode-put-value-local+        (instruction %put-value-local 2))
              (#.+opcode-put-value-stack+        (instruction %put-value-stack 2))
              ;; Program
              (#.+opcode-get-structure+          (instruction %get-structure 2))
              (#.+opcode-get-variable-local+     (instruction %get-variable-local 2))
              (#.+opcode-get-variable-stack+     (instruction %get-variable-stack 2))
              (#.+opcode-get-value-local+        (instruction %get-value-local 2))
              (#.+opcode-get-value-stack+        (instruction %get-value-stack 2))
              ;; Subterm
              (#.+opcode-subterm-variable-local+ (instruction %subterm-variable-local 1))
              (#.+opcode-subterm-variable-stack+ (instruction %subterm-variable-stack 1))
              (#.+opcode-subterm-value-local+    (instruction %subterm-value-local 1))
              (#.+opcode-subterm-value-stack+    (instruction %subterm-value-stack 1))
              (#.+opcode-subterm-void+           (instruction %subterm-void 1))
              ;; Constant
              (#.+opcode-put-constant+           (instruction %put-constant 2))
              (#.+opcode-get-constant+           (instruction %get-constant 2))
              (#.+opcode-subterm-constant+       (instruction %subterm-constant 1))
              ;; List
              (#.+opcode-put-list+               (instruction %put-list 1))
              (#.+opcode-get-list+               (instruction %get-list 1))
              ;; Choice
              (#.+opcode-try+                    (instruction %try 1))
              (#.+opcode-retry+                  (instruction %retry 1))
              (#.+opcode-trust+                  (instruction %trust 0))
              (#.+opcode-cut+                    (instruction %cut 0))
              ;; Control
              (#.+opcode-allocate+               (instruction %allocate 1))
              (#.+opcode-deallocate+             (instruction %deallocate 0))
              ;; need to skip the PC increment for PROC/CALL/JUMP/DONE
              ;; TODO: this is (still) still ugly
              (#.+opcode-proceed+
               (instruction %proceed 0)
               (setf increment-pc nil))
              (#.+opcode-jump+
               (instruction %jump 1)
               (setf increment-pc nil))
              (#.+opcode-call+
               (instruction %call 1)
               (setf increment-pc nil))
              (#.+opcode-dynamic-jump+
               (instruction %dynamic-jump 0)
               (setf increment-pc nil))
              (#.+opcode-dynamic-call+
               (instruction %dynamic-call 0)
               (setf increment-pc nil))
              (#.+opcode-done+
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
            (when (>= pc (wam-code-pointer wam))
              (error "Fell off the end of the program code store."))))))
    (values)))

(defun* run-query ((wam wam)
                   term
                   &key
                   ((result-function function)
                    (lambda (results) (declare (ignore results))))
                   ((status-function function)
                    (lambda (failp) (declare (ignore failp)))))
  "Compile query `term` and run the instructions on the `wam`.

  Resets the heap, etc before running.

  When `*step*` is true, break into the debugger before calling the procedure and
  after each instruction.

  "
  (let ((vars (compile-query wam term)))
    (wam-reset! wam)
    (setf (wam-program-counter wam) 0
          (wam-continuation-pointer wam) +code-sentinel+)
    (run wam (lambda ()
               (funcall result-function
                        (extract-query-results wam vars))))
    (when status-function
      (funcall status-function (wam-fail wam))))
  (values))


