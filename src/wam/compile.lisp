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
  (find register assignments :key #'car))


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


(defun relocate-register (assignments from to)
  "Relocate a register in the assignment list."
  ;; Takes an assignment list like:
  ;;
  ;;   (0 . 2)       ; A0 = X2
  ;;   (1 . (f 2 3)) ; A1 = f(X2, X3)
  ;;   (2 . :foo)    ; X2 = Foo
  ;;   (3 . :bar)    ; X3 = Bar
  (assert (< to from) (from to)
    "Cannot relocate register ~D to ~D, destination must be before source."
    from to)
  (assert (not (tree-member-p to assignments)) (to)
    "Cannot relocate register ~D to ~D in ~S, destination is already in use."
    from to assignments)
  (when assignments
    (map-tree (lambda (r)
                (if (numberp r)
                  (cond ((= r from) to) ; relocate the actual register
                        ((> r from) (1- r)) ; decrement higher registers
                        ((< r from) r)) ; pass through lower registers
                  r))
              assignments)))


(defun parse-term (term)
  "Parse a term into a series of register assignments.

  Return the assignment list, the root functor, and the root functor's arity.

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
         (registers (make-array 64 :fill-pointer arity :adjustable t)))
    (labels
        ((variable-p (term)
           (keywordp term))
         (parse-variable (var)
           ;; If we've already seen this variable, just return its position,
           ;; otherwise allocate a register for it.
           (or (position var registers)
               (vector-push-extend var registers)))
         (parse-structure (structure)
           (let* ((functor (first structure))
                  (arguments (rest structure))
                  (contents (list functor)))
             (prog1
                 (vector-push-extend contents registers)
               ;; Parse the arguments and splice the results into this cell
               ;; once we're finished.  The children should handle extending
               ;; the registers as needed.
               (nconc contents (mapcar #'parse arguments)))))
         (parse (term)
           (cond
             ((variable-p term) (parse-variable term))
             ((symbolp term) (parse (list term))) ; f -> f/0
             ((listp term) (parse-structure term))
             (t (error "Cannot parse term ~S." term)))))
      ;; Arguments are handled specially.  We parse the children as normal,
      ;; and then fill in the argument registers after each child.
      (loop :for argument :in arguments
            :for i :from 0
            :do (setf (aref registers i)
                      (parse argument)))
      (values (loop :for i :from 0 ; turn the register array into an assignment list
                    :for reg :across registers
                    :collect (cons i reg))
              predicate
              arity))))


(defun inline-structure-argument-assignments (assignments functor arity)
  "Inline structure register assignments directly into the argument registers."
  ;; After parsing the term we end up with something like:
  ;;
  ;;   (0 . 2)       ; A0 = X2
  ;;   (1 . 4)       ; A1 = X3    <---------+
  ;;   (2 . :foo)    ; X2 = Foo             | inline this
  ;;   (3 . (f 2 4)) ; X3 = f(X2, X4) ------+
  ;;   (4 . :bar)    ; X4 = Bar
  ;;
  ;; We want to "inline" any structure arguments into the argument registers.
  (labels
      ((recur (remaining assignments)
         (if (zerop remaining)
           assignments
           (let* ((argument-register (car assignments))
                  (argument-number (car argument-register))
                  (argument-target (cdr argument-register)))
             (if (structure-register-p argument-target assignments)
               (recur (1- remaining)
                      (relocate-register (cdr assignments)
                                         argument-target
                                         argument-number))
               (cons argument-register
                     (recur (1- remaining)
                            (cdr assignments))))))))
    (values (sort (recur arity assignments) #'< :key #'car)
            functor
            arity)))


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


(defun flatten (assignments functor arity)
  "Flatten the set of register assignments into a minimal set.

  We remove the plain old variable assignments (in non-argument registers)
  because they're not actually needed in the end.

  "
  (values (-<> assignments
            (topological-sort <> (find-dependencies assignments) :key #'car)
            (remove-if #'variable-assignment-p <>))
          functor
          arity))

(defun flatten-query (registers functor arity)
  (flatten registers functor arity))

(defun flatten-program (registers functor arity)
  (multiple-value-bind (assignments functor arity)
      (flatten registers functor arity)
    (values (reverse assignments)
            functor
            arity)))


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

(defun tokenize-assignments (assignments functor arity)
  "Tokenize a flattened set of register assignments into a stream."
  (values
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
              "Cannot tokenize register assignment to non-argument register ~D in ~A/~D:~%~S."
              argument-register functor arity assignments)
            (list (list :argument argument-register target-register)))
          ;; Otherwise it's a structure assignment.  We know the others have
          ;; gotten flattened away by now.
          (destructuring-bind (register . (functor . arguments)) ass
            (cons (list :structure register functor (length arguments))
                  arguments))))
      assignments)
    functor
    arity))


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

(defun compile-tokens (wam tokens store mode)
  "Generate a series of machine instructions from a stream of tokens.

  The compiled instructions will be appended to `store` using
  `vector-push-extend.`

  "
  (let ((seen (list)))
    (flet ((handle-argument (register target)
             ; OP X_n A_i
             (vector-push-extend-all store
               (if (push-if-new target seen)
                 (ecase mode
                   (:program +opcode-get-variable+)
                   (:query +opcode-put-variable+))
                 (ecase mode
                   (:program +opcode-get-value+)
                   (:query +opcode-put-value+)))
               target
               register))
           (handle-structure (register functor arity)
             ; OP functor reg
             (push register seen)
             (vector-push-extend-all store
               (ecase mode
                 (:program +opcode-get-structure+)
                 (:query +opcode-put-structure+))
               (wam-ensure-functor-index wam (cons functor arity))
               register))
           (handle-register (register)
             ; OP reg
             (vector-push-extend-all store
               (if (push-if-new register seen)
                 (ecase mode
                   (:program +opcode-unify-variable+)
                   (:query +opcode-set-variable+))
                 (ecase mode
                   (:program +opcode-unify-value+)
                   (:query +opcode-set-value+)))
               register)))
      (loop :for token :in tokens :collect
            (match token
              (`(:argument ,register ,target)
               (handle-argument register target))
              (`(:structure ,register ,functor ,arity)
               (handle-structure register functor arity))
              (register (handle-register register)))
            ))))

(defun compile-query-tokens (wam tokens functor arity store)
  (compile-tokens wam tokens store :query)
  (vector-push-extend-all store
    +opcode-call+
    (wam-ensure-functor-index wam (cons functor arity))))

(defun compile-program-tokens (wam tokens functor arity store)
  ; todo: add functor/arity into labels
  (compile-tokens wam tokens store :program)
  (vector-push-extend +opcode-proceed+ store))


;;;; UI
(defun compile-query (wam term)
  "Parse a Lisp query term into a series of WAM machine instructions.

  The compiled code will be returned in a fresh array.

  "
  (let ((code (make-array 64
                          :fill-pointer 0
                          :adjustable t
                          :element-type 'code-word)))
    (multiple-value-bind (tokens functor arity)
        (-<>> term
          parse-term
          (multiple-value-call #'inline-structure-argument-assignments)
          (multiple-value-call #'flatten-query)
          (multiple-value-call #'tokenize-assignments))
      (compile-query-tokens wam tokens functor arity code))
    code))

(defun compile-program (wam term)
  "Parse a Lisp program term into a series of WAM machine instructions.

  The compiled code will be placed at the top of the WAM code store.

  "
  (multiple-value-bind (tokens functor arity)
      (-<>> term
        parse-term
        (multiple-value-call #'inline-structure-argument-assignments)
        (multiple-value-call #'flatten-program)
        (multiple-value-call #'tokenize-assignments))
    (compile-program-tokens wam tokens functor arity (wam-code wam))))

