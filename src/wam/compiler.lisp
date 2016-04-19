(in-package #:bones.wam)
(named-readtables:in-readtable :fare-quasiquote)

;;;; Registers
(deftype register-type ()
  '(member :argument :local :permanent))

(deftype register-number ()
  '(integer 0))


(defclass register ()
  ((type
     :initarg :type
     :reader register-type
     :type register-type)
   (number
     :initarg :number
     :reader register-number
     :type register-number)))


(defun* make-register ((type register-type) (number register-number))
  (:returns register)
  (make-instance 'register :type type :number number))

(defun* make-temporary-register ((number register-number) (arity arity))
  (:returns register)
  (make-register (if (< number arity) :argument :local)
                 number))

(defun* make-permanent-register ((number register-number) (arity arity))
  (:returns register)
  (declare (ignore arity))
  (make-register :permanent number))


(defun* register-to-string ((register register))
  (format nil "~A~D"
          (ecase (register-type register)
            (:argument #\A)
            (:local #\X)
            (:permanent #\Y))
          (+ (register-number register)
             (if *off-by-one* 1 0))))

(defmethod print-object ((object register) stream)
  (print-unreadable-object (object stream :identity nil :type nil)
    (format stream (register-to-string object))))


(defun* register-temporary-p ((register register))
  (member (register-type register) '(:argument :local)))

(defun* register-permanent-p ((register register))
  (eql (register-type register) :permanent))


(defun* register= ((r1 register) (r2 register))
  (:returns boolean)
  (ensure-boolean
    (and (eql (register-type r1)
              (register-type r2))
         (= (register-number r1)
            (register-number r2)))))

(defun* register≈ ((r1 register) (r2 register))
  (:returns boolean)
  (ensure-boolean
    (and (or (eql (register-type r1)
                  (register-type r2))
             ;; local and argument registers are actually the same register,
             ;; just named differently
             (and (register-temporary-p r1)
                  (register-temporary-p r2)))
         (= (register-number r1)
            (register-number r2)))))


;;;; Register Assignments
(deftype register-assignment ()
  ;; A register assignment represented as a cons of (register . contents).
  '(cons register t))

(deftype register-assignment-list ()
  '(trivial-types:association-list register t))


(defun* pprint-assignments ((assignments register-assignment-list))
  (format t "~{~A~%~}"
          (loop :for (register . contents) :in assignments :collect
                (format nil "~A <- ~S" (register-to-string register) contents))))

(defun* find-assignment ((register register)
                         (assignments register-assignment-list))
  (:returns register-assignment)
  "Find the assignment for the given register number in the assignment list."
  (assoc register assignments))


(defun* variable-p (term)
  (:returns boolean)
  (ensure-boolean (keywordp term)))


(defun* variable-assignment-p ((assignment register-assignment))
  "Return whether the register assigment is a simple variable assignment.

  E.g. `X1 = Foo` is simple, but `X2 = f(...)` is not.

  Note that register assignments actually look like `(1 . contents)`, so
  a simple variable assignment would be `(1 . :foo)`.

  "
  (:returns boolean)
  (variable-p (cdr assignment)))

(defun* variable-register-p ((register register)
                             (assignments register-assignment-list))
  (:returns boolean)
  "Return whether the given register contains a variable assignment."
  (variable-assignment-p (find-assignment register assignments)))


(defun* register-assignment-p ((assignment register-assignment))
  (:returns boolean)
  "Return whether the register assigment is a register-to-register assignment.

  E.g. `A1 = X2`.

  Note that this should only ever happen for argument registers.

  "
  (typep (cdr assignment) 'register))


(defun* structure-assignment-p ((assignment register-assignment))
  (:returns boolean)
  "Return whether the given assignment pair is a structure assignment."
  (listp (cdr assignment)))

(defun* structure-register-p ((register register)
                              (assignments register-assignment-list))
  (:returns boolean)
  "Return whether the given register contains a structure assignment."
  (structure-assignment-p (find-assignment register assignments)))


;;;; Parsing
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
;;; We now return you to your regularly scheduled Lisp code.

(defun parse-term (term permanent-variables
                   ;; JESUS TAKE THE WHEEL
                   &optional reserved-variables reserved-arity)
  "Parse a term into a series of register assignments.

  Returns:

    * The assignment list
    * The root functor
    * The root functor's arity

  "
  (let* ((predicate (first term))
         (arguments (rest term))
         (arity (length arguments))
         ;; Preallocate enough registers for all of the arguments.  We'll fill
         ;; them in later.  Note that things are more complicated in the head
         ;; and first body term of a clause (see above).
         (local-registers (make-array 64
                            :fill-pointer (or reserved-arity arity)
                            :adjustable t
                            :initial-element nil))
         ;; We essentially "preallocate" all the permanent variables up front
         ;; because we need them to always be in the same stack registers across
         ;; all the terms of our clause.
         ;;
         ;; The ones that won't get used in this term will end up getting
         ;; flattened away anyway.
         (stack-registers (make-array (length permanent-variables)
                            :initial-contents permanent-variables)))
    (loop :for variable :in reserved-variables :do
          (vector-push-extend variable local-registers))
    (labels
        ((find-variable (var)
           (let ((r (position var local-registers))
                 (s (position var stack-registers)))
             (cond
               (r (make-temporary-register r arity))
               (s (make-permanent-register s arity))
               (t nil))))
         (store-variable (var)
           (make-temporary-register
             (vector-push-extend var local-registers)
             arity))
         (parse-variable (var)
           ;; If we've already seen this variable just return the register it's
           ;; in, otherwise allocate a register for it and return that.
           (or (find-variable var)
               (store-variable var)))
         (parse-structure (structure reg)
           (destructuring-bind (functor . arguments) structure
             ;; If we've been given a register to hold this structure (i.e.
             ;; we're parsing a top-level argument) use it.  Otherwise allocate
             ;; a fresh one.  Note that structures always live in local
             ;; registers, never permanent ones.
             (let ((reg (or reg (vector-push-extend nil local-registers))))
               (setf (aref local-registers reg)
                     (cons functor (mapcar #'parse arguments)))
               (make-temporary-register reg arity))))
         (parse (term &optional register)
           (cond
             ((variable-p term) (parse-variable term))
             ((symbolp term) (parse (list term) register)) ; f -> f/0
             ((listp term) (parse-structure term register))
             (t (error "Cannot parse term ~S." term))))
         (make-assignment-list (registers register-maker)
           (loop :for i :from 0
                 :for contents :across registers
                 :when contents :collect ; don't include unused reserved regs
                 (cons (funcall register-maker i arity)
                       contents))))
      ;; Arguments are handled specially.  We parse the children as normal,
      ;; and then fill in the argument registers after each child.
      (loop :for argument :in arguments
            :for i :from 0
            :for parsed = (parse argument i)
            ;; If the argument didn't fill itself in (structure), do it.
            :when (not (aref local-registers i))
            :do (setf (aref local-registers i) parsed))
      (values (append
                (make-assignment-list local-registers #'make-temporary-register)
                (make-assignment-list stack-registers #'make-permanent-register))
              predicate
              arity))))


;;;; Flattening
;;; "Flattening" is the process of turning a series of register assignments into
;;; a sorted sequence appropriate for turning into a series of instructions.
;;;
;;; The order depends on whether we're compiling a query term or a program term.
;;;
;;; It's a stupid name because the assignments are already flattened as much as
;;; they ever will be.  "Sorting" would be a better name.  Maybe I'll change it
;;; once I'm done with the book.
;;;
;;; Turns:
;;;
;;;   X0 <- p(X1, X2)
;;;   X1 <- A
;;;   X2 <- q(X1, X3)
;;;   X3 <- B
;;;
;;; into something like:
;;;
;;;   X2 <- q(X1, X3), X0 <- p(X1, X2)

(defun find-dependencies (assignments)
  "Return a list of dependencies amongst the given registers.

  Each entry will be a cons of `(a . b)` if register `a` depends on `b`.

  "
  (mapcan
    (lambda (assignment)
      (cond
        ; Variable assignments (X1 <- Foo) don't depend on anything else.
        ((variable-assignment-p assignment)
         ())
        ; Register assignments (A0 <- X5) have one obvious dependency.
        ((register-assignment-p assignment)
         (destructuring-bind (argument . contents) assignment
           (list `(,contents . ,argument))))
        ; Structure assignments depend on all the functor's arguments.
        ((structure-assignment-p assignment)
         (destructuring-bind (target . (functor . reqs))
             assignment
           (declare (ignore functor))
           (loop :for req :in reqs
                 :collect (cons req target))))
        (t (error "Cannot find dependencies for assignment ~S." assignment))))
    assignments))


(defun flatten (assignments)
  "Flatten the set of register assignments into a minimal set.

  We remove the plain old variable assignments (in non-argument registers)
  because they're not actually needed in the end.

  "
  (-<> assignments
    (topological-sort <> (find-dependencies assignments)
                      :key #'car
                      :key-test #'register=
                      :test #'eql)
    (remove-if #'variable-assignment-p <>)))

(defun flatten-query (assignments)
  (flatten assignments))

(defun flatten-program (assignments)
  (reverse (flatten assignments)))


;;;; Tokenization
;;; Tokenizing takes a flattened set of assignments and turns it into a stream
;;; of structure assignments and bare registers.
;;;
;;; It turns:
;;;
;;;   X2 <- q(X1, X3)
;;;   X0 <- p(X1, X2)
;;;   A3 <- X4
;;;
;;; into something like:
;;;
;;;   (X2 = q/2), X1, X3, (X0 = p/2), X1, X2, (A3 = X4)

(defun tokenize-assignments (assignments)
  "Tokenize a flattened set of register assignments into a stream."
  (mapcan
    (lambda (ass)
      ;; Take a single assignment like:
      ;;   X1 = f(X4, Y1)         (X1 . (f X4 Y1))
      ;;   A0 = X5                (A0 . X5)
      ;;
      ;; And turn it into a stream of tokens:
      ;;   (X1 = f/2), X4, Y1      ((:structure X1 f 2) X4 Y1
      ;;   (A0 = X5)                (:argument A0 X5))
      (if (register-assignment-p ass)
        ;; It might be a register assignment for an argument register.
        (destructuring-bind (argument-register . target-register) ass
          (list (list :argument argument-register target-register)))
        ;; Otherwise it's a structure assignment.  We know the others have
        ;; gotten flattened away by now.
        (destructuring-bind (register . (functor . arguments)) ass
          (cons (list :structure register functor (length arguments))
                arguments))))
    assignments))


(defun tokenize-term
    (term permanent-variables reserved-variables reserved-arity flattener)
  (multiple-value-bind (assignments functor arity)
      (parse-term term permanent-variables reserved-variables reserved-arity)
    (values (->> assignments
              (funcall flattener)
              tokenize-assignments)
            functor
            arity)))

(defun tokenize-program-term
    (term permanent-variables reserved-variables reserved-arity)
  "Tokenize `term` as a program term, returning its tokens, functor, and arity."
  (tokenize-term term
                 permanent-variables
                 reserved-variables
                 reserved-arity
                 #'flatten-program))

(defun tokenize-query-term
    (term permanent-variables &optional reserved-variables reserved-arity)
  "Tokenize `term` as a query term, returning its stream of tokens."
  (multiple-value-bind (tokens functor arity)
      (tokenize-term term
                     permanent-variables
                     reserved-variables
                     reserved-arity
                     #'flatten-query)
    ;; We need to shove a CALL token onto the end.
    (append tokens `((:call ,functor ,arity)))))


;;;; Bytecode
;;; Once we have a tokenized stream we can generate the machine instructions
;;; from it.
;;;
;;; We turn:
;;;
;;;   (X2 = q/2), X1, X3, (X0 = p/2), X1, X2
;;;
;;; into something like:
;;;
;;;   (#'%put-structure 2 q 2)
;;;   (#'%set-variable 1)
;;;   (#'%set-variable 3)
;;;   (#'%put-structure 0 p 2)
;;;   (#'%set-value 1)
;;;   (#'%set-value 2)

(defun find-opcode (opcode newp mode &optional register)
  (flet ((find-variant (register)
           (when register
             (if (register-temporary-p register)
               :local
               :stack))))
    (eswitch ((list opcode newp mode (find-variant register)) :test #'equal)
      ('(:argument t   :program :local) +opcode-get-variable-local+)
      ('(:argument t   :program :stack) +opcode-get-variable-stack+)
      ('(:argument t   :query   :local) +opcode-put-variable-local+)
      ('(:argument t   :query   :stack) +opcode-put-variable-stack+)
      ('(:argument nil :program :local) +opcode-get-value-local+)
      ('(:argument nil :program :stack) +opcode-get-value-stack+)
      ('(:argument nil :query   :local) +opcode-put-value-local+)
      ('(:argument nil :query   :stack) +opcode-put-value-stack+)
      ;; Structures can only live locally, they never go on the stack
      ('(:structure nil :program :local) +opcode-get-structure-local+)
      ('(:structure nil :query   :local) +opcode-put-structure-local+)
      ('(:register t   :program :local) +opcode-unify-variable-local+)
      ('(:register t   :program :stack) +opcode-unify-variable-stack+)
      ('(:register t   :query   :local) +opcode-set-variable-local+)
      ('(:register t   :query   :stack) +opcode-set-variable-stack+)
      ('(:register nil :program :local) +opcode-unify-value-local+)
      ('(:register nil :program :stack) +opcode-unify-value-stack+)
      ('(:register nil :query   :local) +opcode-set-value-local+)
      ('(:register nil :query   :stack) +opcode-set-value-stack+))))

(defun compile-tokens (wam head-tokens body-tokens store)
  "Generate a series of machine instructions from a stream of head and body
  tokens.

  The `head-tokens` should be program-style tokens, and are compiled in program
  mode.  The `body-tokens` should be query-style tokens, and are compiled in
  query mode.

  Actual queries are a special case where the `head-tokens` stream is `nil`

  The compiled instructions will be appended to `store` using
  `code-push-instructions!`.

  "
  (let ((seen (list))
        (mode nil))
    (labels
        ((handle-argument (argument-register source-register)
           ;; OP X_n A_i
           (let ((newp (push-if-new source-register seen :test #'register=)))
             (code-push-instruction! store
                 (find-opcode :argument newp mode source-register)
               (register-number source-register)
               (register-number argument-register))))
         (handle-structure (destination-register functor arity)
           ;; OP functor reg
           (push destination-register seen)
           (code-push-instruction! store
               (find-opcode :structure nil mode destination-register)
             (wam-ensure-functor-index wam (cons functor arity))
             (register-number destination-register)))
         (handle-call (functor arity)
           ;; CALL functor
           (code-push-instruction! store
               +opcode-call+
             (wam-ensure-functor-index wam (cons functor arity))))
         (handle-register (register)
           ;; OP reg
           (let ((newp (push-if-new register seen :test #'register=)))
             (code-push-instruction! store
                 (find-opcode :register newp mode register)
               (register-number register))))
         (handle-stream (tokens)
           (loop :for token :in tokens :collect
                 (ematch token
                   ((guard `(:argument ,argument-register ,source-register)
                           (and (eql (register-type argument-register) :argument)
                                (member (register-type source-register)
                                        '(:local :permanent))))
                    (handle-argument argument-register source-register))
                   ((guard `(:structure ,destination-register ,functor ,arity)
                           (member (register-type destination-register)
                                   '(:local :argument)))
                    (handle-structure destination-register functor arity))
                   (`(:call ,functor ,arity)
                    (handle-call functor arity))
                   ((guard register
                           (typep register 'register))
                    (handle-register register))))))
      (when head-tokens
        (setf mode :program)
        (handle-stream head-tokens))
      (setf mode :query)
      (handle-stream body-tokens))))


;;;; UI
(defun find-variables (terms)
  "Return the set of variables in `terms`."
  (remove-duplicates (tree-collect #'variable-p terms)))

(defun find-shared-variables (terms)
  "Return the set of all variables shared by two or more terms."
  (labels
      ((count-uses (variable)
         (count-if (curry #'tree-member-p variable) terms))
       (shared-p (variable)
         (> (count-uses variable) 1)))
    (remove-if-not #'shared-p (find-variables terms))))

(defun find-permanent-variables (clause)
  "Return a list of all the permanent variables in `clause`.

  Permanent variables are those that appear in more than one goal of the clause,
  where the head of the clause is considered to be a part of the first goal.

  "
  (if (<= (length clause) 2)
    (list) ; facts and chain rules have no permanent variables at all
    (destructuring-bind (head body-first . body-rest) clause
      ;; the head is treated as part of the first goal for the purposes of
      ;; finding permanent variables
      (find-shared-variables (cons (cons head body-first) body-rest)))))

(defun find-head-variables (clause)
  (if (<= (length clause) 1)
    (list)
    (destructuring-bind (head body-first . body-rest) clause
      (declare (ignore body-rest))
      (find-shared-variables (list head body-first)))))


(defun mark-label (wam functor arity store)
  "Set the code label `(functor . arity)` to point at the next space in `store`."
  ;; todo make this less ugly
  (setf (wam-code-label wam (wam-ensure-functor-index wam (cons functor arity)))
        (fill-pointer store)))


(defun make-query-code-store ()
  (make-array 64
    :fill-pointer 0
    :adjustable t
    :element-type 'code-word))


(defun compile-clause (wam store head body)
  "Compile the clause directly into `store` and return the permanent variables.

  `head` should be the head of the clause for program clauses, or `nil` for
  query clauses.

  `body` is the body of the clause, or `nil` for facts.

  "
  (let* ((permanent-variables
           (if (null head)
             ;; For query clauses we cheat a bit and make ALL variables
             ;; permanent, so we can extract their bindings as results later.
             (find-variables body)
             (find-permanent-variables (cons head body))))
         (head-variables
           (set-difference (find-head-variables (cons head body))
                           permanent-variables))
         (head-arity
           (max (1- (length head))
                (1- (length (car body)))))
         (head-tokens
           (when head
             (multiple-value-bind (tokens functor arity)
                 (tokenize-program-term head
                                        permanent-variables
                                        head-variables
                                        head-arity)
               (mark-label wam functor arity store) ; TODO: this is ugly
               tokens)))
         (body-tokens
           (when body
             (append
               (tokenize-query-term (first body)
                                    permanent-variables
                                    head-variables
                                    head-arity)
               (loop :for term :in (rest body) :append
                     (tokenize-query-term term
                                          permanent-variables))))))
    (flet ((compile% () (compile-tokens wam head-tokens body-tokens store)))
      ;; We need to compile facts and rules differently.  Facts end with
      ;; a PROCEED and rules are wrapped in ALOC/DEAL.
      (cond
        ((and head body) ; a full-ass rule
         (code-push-instruction! store +opcode-allocate+ (length permanent-variables))
         (compile%)
         (code-push-instruction! store +opcode-deallocate+))
        ((and head (null body)) ; a bare fact
         (compile%)
         (code-push-instruction! store +opcode-proceed+))
        (t ; a query
         ;; The book doesn't have this ALOC here, but we do it to aid in result
         ;; extraction.  Basically, to make extracting th results of a query
         ;; easier we allocate all of its variables on the stack, so we need
         ;; push a stack frame for them before we get started.  We don't DEAL
         ;; because we want the frame to be left on the stack at the end so we
         ;; can poke at it.
         (code-push-instruction! store +opcode-allocate+ (length permanent-variables))
         (compile%)
         (code-push-instruction! store +opcode-done+))))
    permanent-variables))

(defun compile-query (wam query)
  "Compile `query` into a fresh array of bytecode.

  `query` should be a list of goal terms.

  Returns the fresh code array and the permanent variables.

  "
  (let* ((store (make-query-code-store))
         (permanent-variables (compile-clause wam store nil query)))
    (values store permanent-variables)))

(defun compile-program (wam rule)
  "Compile `rule` into the WAM's code store.

  `rule` should be a clause consisting of a head term and zero or more body
  terms.  A rule with no body is called a fact.

  "
  (compile-clause wam (wam-code wam) (first rule) (rest rule))
  (values))

