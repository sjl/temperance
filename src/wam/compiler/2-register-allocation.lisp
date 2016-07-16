(in-package #:bones.wam)

;;;; .-,--.               .               ,.   .  .              .
;;;;  `|__/ ,-. ,-. . ,-. |- ,-. ,-.     / |   |  |  ,-. ,-. ,-. |- . ,-. ,-.
;;;;  )| \  |-' | | | `-. |  |-' |      /~~|-. |  |  | | |   ,-| |  | | | | |
;;;;  `'  ` `-' `-| ' `-' `' `-' '    ,'   `-' `' `' `-' `-' `-^ `' ' `-' ' '
;;;;             ,|
;;;;             `'

;;; You might want to grab a coffee for this one.
;;;
;;; Consider this simple Prolog example: `p(A, q(A, r(B)))`.  We're going to get
;;; this as a Lisp list: `(p :a (q :a (r b)))`.
;;;
;;; The goal is to turn this list into a set of register assignments.  The book
;;; handwaves around how to do this, and it turns out to be pretty complicated.
;;; This example will (maybe, read on) be turned into:
;;;
;;;     A0 <- X2
;;;     A1 <- (q X2 X3)
;;;     X2 <- :a
;;;     X3 <- (r X4)
;;;     X4 <- :b
;;;
;;; There are a few things to note here.  First: like the book says, the
;;; outermost predicate is stripped off and returned separately (later it'll be
;;; used to label the code for a program, or to figure out the procedure to call
;;; for a query).
;;;
;;; The first N registers are designated as argument registers.  Structure
;;; assignments can live directly in the argument registers, but variables
;;; cannot.  In the example above we can see that A1 contains a structure
;;; assignment.  However, the variable `:a` doesn't live in A0 -- it lives in
;;; X2, which A0 points at.  The books neglects to explain this little fact.
;;;
;;; The next edge case is permanent variables, which the book does talk about.
;;; Permanent variables are allocated to stack registers, so if `:b` was
;;; permanent in our example we'd get:
;;;
;;;     A0 <- X2
;;;     A1 <- (q X2 X3)
;;;     X2 <- :a
;;;     X3 <- (r Y0)
;;;     Y0 <- :b
;;;
;;; Note that the mapping of permanent variables to stack register numbers has
;;; to be consistent as we compile all the terms in a clause, so we cheat a bit
;;; here and just always add them all, in order, to the register assignment
;;; produced when parsing.  They'll get flattened away later anyway -- it's the
;;; USES that we actually care about.  In our example, the `Y0 <- :b` will get
;;; flattened away, but the USE of Y0 in X3 will remain).
;;;
;;; We're almost done, I promise, but there's one more edge case to deal with.
;;;
;;; When we've got a clause with a head and at least one body term, we need the
;;; head term and the first body term to share argument/local registers.  For
;;; example, if we have the clause `p(Cats) :- q(A, B, C, Cats)` then when
;;; compiling the head `(p :cats)` we want to get:
;;;
;;;     A0 <- X4
;;;     A1 <- ???
;;;     A2 <- ???
;;;     A3 <- ???
;;;     X4 <- :cats
;;;
;;; And when compiling `(q :a :b :c :cats)` we need:
;;;
;;;     A0 <- X5
;;;     A1 <- X6
;;;     A2 <- X7
;;;     A3 <- X4
;;;     X4 <- :cats
;;;     X5 <- :a
;;;     X6 <- :b
;;;     X7 <- :c
;;;
;;; What the hell are those empty argument registers in p?  And why did we order
;;; the X registers of q like that?
;;;
;;; The book does not bother to mention this important fact at all, so to find
;;; out that you have to handle this you need to do the following:
;;;
;;; 1. Implement it without this behavior.
;;; 2. Notice your results are wrong.
;;; 3. Figure out the right bytecode on a whiteboard.
;;; 4. Try to puzzle out why that bytecode isn't generated when you do exactly
;;;    what the book says.
;;; 5. Scour IRC and the web for scraps of information on what the hell you need
;;;    to do here.
;;; 6. Find the answer in a comment squirreled away in a source file somewhere
;;;    in a language you don't know.
;;; 7. Drink.
;;;
;;; Perhaps you're reading this comment as part of step 6 right now.  If so:
;;; welcome aboard.  Email me and we can swap horror stories about this process
;;; over drinks some time.
;;;
;;; Okay, so the clause head and first body term need to share argument/local
;;; registers.  Why?  To understand this, we need to go back to what Prolog
;;; clauses are supposed to do.
;;;
;;; Imagine we have:
;;;
;;;     p(f(X)) :- q(X), ...other goals.
;;;
;;; When we want to check if `p(SOMETHING)` is true, we need to first unify
;;; SOMETHING with `f(X)`.  Then we search all of the goals in the body, AFTER
;;; substituting in any X's in those goals with the X from the result of the
;;; unification.
;;;
;;; This substitution is why we need the head and the first term in the body to
;;; share the same argument/local registers.  By sharing the registers, when the
;;; body term builds a representation of itself on the stack before calling its
;;; predicate any references to X will be point at the (unified) results instead
;;; of fresh ones (because they'll be compiled as `put_value` instead of
;;; `put_variable`).
;;;
;;; But wait: don't we need to substitute into ALL the body terms, not just the
;;; first one?  Yes we do, but the trick is that any variables in the REST of
;;; the body that would need to be substituted must, by definition, be permanent
;;; variables!  So the substitution process for the rest of the body is handled
;;; automatically with the stack machinery.
;;;
;;; In theory, you could eliminate this edge case by NOT treating the head and
;;; first goal as a single term when searching for permanent variables.  Then
;;; all substitution would happen elegantly through the stack.  But this
;;; allocates more variables on the stack than you really need (especially for
;;; rules with just a single term in the body (which is many of them)), so we
;;; have this extra corner case to optimize it away.
;;;
;;; In the following code these variables will be called "nead variables"
;;; because:
;;;
;;; 1. They're present in the head of the clause.
;;; 2. They're present in the first term of the body (the "neck", as referred to
;;;    in "neck cut" and such).
;;; 3. https://www.urbandictionary.com/define.php?term=nead&defid=1488946
;;;
;;; We now return you to your regularly scheduled Lisp code.


(defstruct allocation-state
  (local-registers (make-queue) :type queue)
  (stack-registers nil :type list)
  (permanent-variables nil :type list)
  (anonymous-variables nil :type list)
  (reserved-variables nil :type list)
  (reserved-arity nil :type (or null arity))
  (actual-arity 0 :type arity))


(defun* find-variable ((state allocation-state) (variable symbol))
  "Return the register that already contains this variable, or `nil` otherwise."
  (or (when-let (r (position variable
                             (queue-contents
                               (allocation-state-local-registers state))))
        (make-temporary-register r (allocation-state-actual-arity state)))
      (when-let (s (position variable
                             (allocation-state-stack-registers state)))
        (make-permanent-register s))
      nil))

(defun* store-variable ((state allocation-state) (variable symbol))
  "Assign `variable` to the next available local register.

  It is assumed that `variable` is not already assigned to another register
  (check that with `find-variable` first).

  It is also assumed that this will be a non-argument register, because as
  mentioned above variables cannot live directly inside argument registers.

  "
  (make-register
    :local
    (1- (enqueue variable (allocation-state-local-registers state)))))

(defun* ensure-variable ((state allocation-state) (variable symbol))
  (or (find-variable state variable)
      (store-variable state variable)))


(defmacro set-when-nil ((accessor instance) value-form)
  (once-only (instance)
    `(when (not (,accessor ,instance))
      (setf (,accessor ,instance) ,value-form))))


(defun* variable-anonymous-p ((state allocation-state) (variable symbol))
  "Return whether `variable` is considered anonymous in `state`."
  (and (member variable (allocation-state-anonymous-variables state)) t))


(defun* allocate-variable-register ((state allocation-state) (variable symbol))
  (if (variable-anonymous-p state variable)
    (make-anonymous-register)
    (ensure-variable state variable)))

(defun* allocate-nonvariable-register ((state allocation-state))
  "Allocate and return a register for something that's not a variable."
  ;; We need to allocate registers for things like structures and lists, but we
  ;; never need to look them up later (like we do with variables), so we'll just
  ;; shove a nil into the local registers array as a placeholder.
  (make-temporary-register
    (enqueue nil (allocation-state-local-registers state))
    (allocation-state-actual-arity state)))


(defgeneric allocate-register (node allocation-state))


(defmethod allocate-register ((node top-level-node) state)
  (declare (ignore node state))
  (values))

(defmethod allocate-register ((node variable-node) state)
  (set-when-nil (node-register node)
                (allocate-variable-register state (node-variable node))))

(defmethod allocate-register ((node argument-variable-node) state)
  (set-when-nil (node-secondary-register node)
                (allocate-variable-register state (node-variable node))))

(defmethod allocate-register ((node structure-node) state)
  (set-when-nil (node-register node)
                (allocate-nonvariable-register state)))

(defmethod allocate-register ((node list-node) state)
  (set-when-nil (node-register node)
                (allocate-nonvariable-register state)))

(defmethod allocate-register ((node lisp-object-node) state)
  (set-when-nil (node-register node)
                (allocate-nonvariable-register state)))


(defun* allocate-argument-registers ((node top-level-node))
  (loop :for argument :in (top-level-node-arguments node)
        :for i :from 0
        :do (setf (node-register argument)
                  (make-register :argument i))))

(defun* allocate-nonargument-registers ((node top-level-node)
                                        (clause-props clause-properties)
                                        &key nead)
  ;; JESUS TAKE THE WHEEL
  (let*
      ((actual-arity (top-level-node-arity node))
       (reserved-arity (when nead
                         (clause-nead-arity clause-props)))
       (reserved-variables (when nead
                             (clause-nead-vars clause-props)))
       (permanent-variables (clause-permanent-vars clause-props))
       (local-registers (make-queue))
       ;; We essentially "preallocate" all the permanent variables up front
       ;; because we need them to always be in the same stack registers across
       ;; all the terms of our clause.
       ;;
       ;; The ones that won't get used in this term will end up getting
       ;; flattened away anyway.
       (stack-registers permanent-variables)
       (allocation-state
         (make-allocation-state
           :local-registers local-registers
           :stack-registers stack-registers
           :permanent-variables permanent-variables
           :anonymous-variables (clause-anonymous-vars clause-props)
           :reserved-variables reserved-variables
           :reserved-arity reserved-arity
           :actual-arity actual-arity)))
    ;; Preallocate enough registers for all of the arguments.  We'll fill
    ;; them in later.  Note that things are more complicated in the head and
    ;; first body term of a clause (see above).
    (loop :repeat (or reserved-arity actual-arity)
          :do (enqueue nil local-registers))
    ;; Actually reserve the reserved (but non-permanent, see above) variables.
    ;; They need to live in consistent spots for the head and first body term.
    (loop :for variable :in reserved-variables
          :do (enqueue variable local-registers))
    (recursively ((remaining (list node)))
      (when remaining
        (destructuring-bind (node . remaining) remaining
          (allocate-register node allocation-state)
          (recur (append remaining (node-children node))))))))

(defun* allocate-registers ((node top-level-node)
                            (clause-props clause-properties)
                            &key nead)
  (allocate-argument-registers node)
  (allocate-nonargument-registers node clause-props :nead nead))


