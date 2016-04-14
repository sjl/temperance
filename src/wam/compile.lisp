(in-package #:bones.wam)
(named-readtables:in-readtable :fare-quasiquote)

;;;; Parsing
;;; Turns p(A, q(A, B)) into something like:
;;;
;;;   X0 -> p(X1, X2)
;;;   X1 -> A
;;;   X2 -> q(X1, X3)
;;;   X3 -> B
;;;
;;; And then processes the argument register assignments into:
;;;
;;;   p/2:
;;;   A0 -> A
;;;   A1 -> q(A1, X3)
;;;   X2 -> B

(defun find-assignment (register assignments)
  "Find the assignment for the given register number in the assignment list."
  (assoc register assignments))


(defun variable-p (term)
  (keywordp term))

(defun find-permanent-variables (clause)
  "Return a list of all the 'permanent' variables in `clause`.

  Permanent variables are those that appear in more than one goal of the clause,
  where the head of the clause is considered to be a part of the first goal.

  "
  (if (< (length clause) 2)
    (list) ; facts and chain rules have no permanent variables at all
    (destructuring-bind (head body-first . body-rest) clause
      ;; the head is treated as part of the first goal for the purposes of
      ;; finding permanent variables
      (let* ((goals (cons (cons head body-first) body-rest))
             (variables (remove-duplicates (tree-collect #'variable-p goals))))
        (flet ((permanent-p (variable)
                 "Permanent variables are those contained in more than 1 goal."
                 (> (count-if (curry #'tree-member-p variable)
                              goals)
                    1)))
          (remove-if-not #'permanent-p variables))))))


(defun variable-assignment-p (ass)
  "Return whether the register assigment is a simple variable assignment.

  E.g. `X1 = Foo` is simple, but `X2 = f(...)` is not.

  Note that register assignments actually look like `(1 . contents)`, so
  a simple variable assignment would be `(1 . :foo)`.

  "
  (keywordp (cdr ass)))

(defun variable-register-p (register assignments)
  "Return whether the given register contains a variable assignment."
  (variable-assignment-p (find-assignment register assignments)))


(defun register-assignment-p (ass)
  "Return whether the register assigment is a register-to-register assignment.

  E.g. `A1 = X2`.

  Note that this should only ever happen for argument registers.

  "
  (numberp (cdr ass)))


(defun structure-assignment-p (ass)
  "Return whether the given assignment pair is a structure assignment."
  (listp (cdr ass)))

(defun structure-register-p (register assignments)
  "Return whether the given register contains a structure assignment."
  (structure-assignment-p (find-assignment register assignments)))


(defun parse-term (term)
  "Parse a term into a series of register assignments.

  Returns:

    * The assignment list
    * The root functor
    * The root functor's arity

  "
  ;; A term is a Lispy representation of the raw Prolog.  A register assignment
  ;; is a cons of (register . assigned-to), e.g.:
  ;;
  ;;   (p :foo (f :foo :bar))
  ;;   ->
  ;;   (0 . 2)       ; A0 = X2
  ;;   (1 . 4)       ; A1 = X3
  ;;   (2 . :foo)    ; X2 = Foo
  ;;   (3 . (f 2 4)) ; X3 = f(X2, X4)
  ;;   (4 . :bar)    ; X4 = Bar
  (let* ((predicate (first term))
         (arguments (rest term))
         (arity (length arguments))
         ;; Preallocate enough registers for all of the arguments.
         ;; We'll fill them in later.
         (registers (make-array 64
                                :fill-pointer arity
                                :adjustable t
                                :initial-element nil)))
    (labels
        ((parse-variable (var)
           ;; If we've already seen this variable, just return its position,
           ;; otherwise allocate a register for it.
           (or (position var registers)
               (vector-push-extend var registers)))
         (parse-structure (structure register)
           (destructuring-bind (functor . arguments) structure
             ;; If we've been given a register to hold this structure (i.e.
             ;; we're parsing a top-level argument, use it.  Otherwise allocate
             ;; a fresh one.
             (let ((register (or register (vector-push-extend nil registers))))
               (setf (aref registers register)
                     (cons functor (mapcar #'parse arguments)))
               register)))
         (parse (term &optional register)
           (cond
             ((variable-p term) (parse-variable term))
             ((symbolp term) (parse (list term) register)) ; f -> f/0
             ((listp term) (parse-structure term register))
             (t (error "Cannot parse term ~S." term)))))
      ;; Arguments are handled specially.  We parse the children as normal,
      ;; and then fill in the argument registers after each child.
      (loop :for argument :in arguments
            :for i :from 0
            :for parsed = (parse argument i)
            ;; If the argument didn't fill itself in (structure), do it.
            :when (not (aref registers i))
            :do (setf (aref registers i) parsed))
      (values (loop :for i :from 0 ; turn the register array into an assignment list
                    :for reg :across registers
                    :collect (cons i reg))
              predicate
              arity))))


(defun register-types (assignments arity permanent-variables)
  "Return the alist of register types for the given register assignments.

  `assignments` must be sorted, and not flattened yet.

  "
  (loop :for i :from 0
        :for (register . contents) :in assignments :collect
        (cons i (cond
                  ((< i arity) :argument)
                  ((member contents permanent-variables) :permanent)
                  (t :local)))))


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
;;;   X0 -> p(X1, X2)
;;;   X1 -> A
;;;   X2 -> q(X1, X3)
;;;   X3 -> B
;;;
;;; into something like:
;;;
;;;   X2 -> q(X1, X3), X0 -> p(X1, X2)

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
         (list (cons (cdr assignment) (car assignment))))
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
    (topological-sort <> (find-dependencies assignments) :key #'car)
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
;;;   X2 -> q(X1, X3), X0 -> p(X1, X2)
;;;
;;; into something like:
;;;
;;;   (X2 = q/2), X1, X3, (X0 = p/2), X1, X2

(defun tokenize-assignments (assignments arity)
  "Tokenize a flattened set of register assignments into a stream."
  (mapcan
    (lambda (ass)
      ;; Take a single assignment like:
      ;;   X1 = f(a, b, c)         (1 . (f a b c))
      ;;   A0 = X5                 (0 . 5)
      ;;
      ;; And turn it into a stream of tokens:
      ;;   (X1 = f/3), a, b, c     ((:structure 1 f 3) a b c)
      ;;   (A0 = X5)               ((:argument 0 5))
      (if (register-assignment-p ass)
        ;; It might be a register assignment for an argument register.
        (destructuring-bind (argument-register . target-register) ass
          (assert (< argument-register arity) ()
            "Cannot tokenize register assignment to non-argument register ~D in ???/~D:~%~S."
            argument-register arity assignments)
          (list (list :argument argument-register target-register)))
        ;; Otherwise it's a structure assignment.  We know the others have
        ;; gotten flattened away by now.
        (destructuring-bind (register . (functor . arguments)) ass
          (cons (list :structure register functor (length arguments))
                arguments))))
    assignments))


(defun zip-register-types (tokens register-types)
  (labels
      ((get-type (register)
         (cdr (assoc register register-types)))
       (update-leaf (leaf)
         (if (numberp leaf)
           (cons (get-type leaf) leaf)
           leaf))
       (fix-token (token)
         (match token
           (`(:structure ,register ,functor ,arity)
            `(:structure (,(get-type register) . ,register)
              ,functor
              ,arity))
           ((guard n (numberp n))
            (update-leaf n))
           (other (map-tree #'update-leaf other)))))
    (mapcar #'fix-token tokens)))


(defun tokenize-term (term permanent-variables flattener)
  (multiple-value-bind (assignments functor arity)
      (parse-term term)
    (let* ((register-types (register-types assignments
                                           arity
                                           permanent-variables))
           (assignments (funcall flattener assignments))
           (tokens (tokenize-assignments assignments arity)))
      (values (zip-register-types tokens register-types)
              functor
              arity))))

(defun tokenize-program-term (term permanent-variables)
  "Tokenize `term` as a program term, returning its tokens, functor, and arity."
  (multiple-value-bind (tokens functor arity)
      (tokenize-term term permanent-variables #'flatten-program)
    ;; We need to shove a PROCEED token onto the end.
    (values (append tokens `((:proceed)))
            functor
            arity)))

(defun tokenize-query-term (term permanent-variables)
  "Tokenize `term` as a query term, returning its stream of tokens."
  (multiple-value-bind (tokens functor arity)
      (tokenize-term term permanent-variables #'flatten-query)
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
        ((handle-argument (argument-type argument source-type source)
           (assert (eql argument-type :argument) ()
             "Attempted argument assignment to non-argument register.")
           (assert (member source-type '(:local :permanent)) ()
             "Attempted argument assignment from non-permanent/local register.")
           ; OP X_n A_i
           (code-push-instruction! store
               (if (push-if-new source seen)
                 (ecase mode
                   (:program +opcode-get-variable+)
                   (:query +opcode-put-variable+))
                 (ecase mode
                   (:program +opcode-get-value+)
                   (:query +opcode-put-value+)))
             source
             argument))
         (handle-structure (register-type register functor arity)
           (assert (member register-type '(:local :argument)) ()
             "Attempted structure assignment to non-local/argument register.")
           ; OP functor reg
           (push register seen)
           (code-push-instruction! store
               (ecase mode
                 (:program +opcode-get-structure+)
                 (:query +opcode-put-structure+))
             (wam-ensure-functor-index wam (cons functor arity))
             register))
         (handle-call (functor arity)
           (code-push-instruction! store
               +opcode-call+
             (wam-ensure-functor-index wam (cons functor arity))))
         (handle-proceed ()
           (code-push-instruction! store
               +opcode-proceed+))
         (handle-register (register-type register)
           (declare (ignore register-type))
           ; OP reg
           (code-push-instruction! store
               (if (push-if-new register seen)
                 (ecase mode
                   (:program +opcode-unify-variable+)
                   (:query +opcode-set-variable+))
                 (ecase mode
                   (:program +opcode-unify-value+)
                   (:query +opcode-set-value+)))
             register))
         (handle-stream (tokens)
           (loop :for token :in tokens :collect
                 (match token
                   (`(:argument (,argument-type . ,argument) (,source-type . ,source))
                    (handle-argument argument-type argument source-type source))
                   (`(:structure (,register-type . ,register) ,functor ,arity)
                    (handle-structure register-type register functor arity))
                   (`(:call ,functor ,arity)
                    (handle-call functor arity))
                   (`(:proceed)
                    (handle-proceed))
                   (`(,register-type . ,register)
                    (handle-register register-type register))))))
      (when head-tokens
        (setf mode :program)
        (handle-stream head-tokens))
      (setf mode :query)
      (handle-stream body-tokens))))

(defun mark-label (wam functor arity store)
  "Set the code label `(functor . arity)` to point at the next space in `store`."
  ;; todo make this less ugly
  (setf (wam-code-label wam (wam-ensure-functor-index wam (cons functor arity)))
        (fill-pointer store)))


;;;; UI
(defun make-query-code-store ()
  (make-array 64
              :fill-pointer 0
              :adjustable t
              :element-type 'code-word))

(defun compile-clause (wam store head body)
  "Compile the clause into the given store array.

  `head` should be the head of the clause for program clauses, or may be `nil`
  for query clauses.

  "
  (let* ((permanent-variables
           (find-permanent-variables (cons head body)))
         (head-tokens
           (when head
             (multiple-value-bind (tokens functor arity)
                 (tokenize-program-term head permanent-variables)
               (mark-label wam functor arity store) ; TODO: this is ugly
               tokens)))
         (body-tokens
           (loop :for term :in body :append
                 (tokenize-query-term term permanent-variables))))
    (compile-tokens wam head-tokens body-tokens store))
  (values))

(defun compile-query (wam query)
  "Compile `query` into a fresh array of bytecode.

  `query` should be a list of goal terms.

  "
  (let ((store (make-query-code-store)))
    (compile-clause wam store nil query)
    store))

(defun compile-program (wam rule)
  "Compile `rule` into the WAM's code store.

  `rule` should be a clause consisting of a head term and zero or more body
  terms.  A rule with no body is also called a \"fact\".

  "
  (compile-clause wam (wam-code wam) (first rule) (rest rule))
  (values))

