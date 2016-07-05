(in-package #:bones.wam)
(named-readtables:in-readtable :fare-quasiquote)


;;;; Utils
(declaim (inline variablep))

(defun* variablep (term)
  (:returns boolean)
  (and (symbolp term)
       (char= (char (symbol-name term) 0) #\?)))


;;;; Registers
(deftype register-type ()
  '(member :argument :local :permanent :anonymous))

(deftype register-number ()
  `(integer 0 ,(1- +register-count+)))


(declaim (inline register-type register-number))
(defstruct (register (:constructor make-register (type number)))
  (type :local :type register-type)
  (number 0 :type register-number))


(defun* make-temporary-register ((number register-number) (arity arity))
  (:returns register)
  (make-register (if (< number arity) :argument :local)
                 number))

(defun* make-permanent-register ((number register-number))
  (:returns register)
  (make-register :permanent number))

(defun* make-anonymous-register ()
  (:returns register)
  (make-register :anonymous 0))


(defun* register-to-string ((register register))
  (if (eq (register-type register) :anonymous)
    "__"
    (format nil "~A~D"
            (ecase (register-type register)
              (:argument #\A)
              (:local #\X)
              (:permanent #\Y))
            (+ (register-number register)
               (if *off-by-one* 1 0)))))

(defmethod print-object ((object register) stream)
  (print-unreadable-object (object stream :identity nil :type nil)
    (format stream (register-to-string object))))


(declaim (inline register-argument-p
                 register-temporary-p
                 register-permanent-p
                 register-anonymous-p))
(defun* register-argument-p ((register register))
  (eq (register-type register) :argument))

(defun* register-temporary-p ((register register))
  (member (register-type register) '(:argument :local)))

(defun* register-permanent-p ((register register))
  (eq (register-type register) :permanent))

(defun* register-anonymous-p ((register register))
  (eq (register-type register) :anonymous))


(declaim (inline register=))
(defun* register= ((r1 register) (r2 register))
  (and (eql (register-type r1)
            (register-type r2))
       (= (register-number r1)
          (register-number r2))))


;;;; Parse Trees
(defclass node () ())

(defclass top-level-node (node)
  ((functor :accessor node-functor
            :type symbol
            :initarg :functor)
   (arity :accessor node-arity
          :type arity
          :initarg :arity)
   (arguments :accessor node-arguments
              :type list
              :initarg :arguments)))

(defclass vanilla-node (node)
  ((register :accessor node-register
             :type register
             :documentation "The register allocated to store this node.")))


(defclass structure-node (vanilla-node)
  ((functor :accessor node-functor
            :type symbol
            :initarg :functor)
   (arity :accessor node-arity
          :type arity
          :initarg :arity)
   (arguments :accessor node-arguments
              :type list
              :initarg :arguments)))

(defclass variable-node (vanilla-node)
  ((variable :accessor node-variable
             :type symbol
             :initarg :variable)))

(defclass argument-variable-node (variable-node)
  ((secondary-register
     :accessor node-secondary-register
     :type register
     :documentation
     "The register that actually holds the variable (NOT the argument register).")))

(defclass list-node (vanilla-node)
  ((head :accessor node-head :type node :initarg :head)
   (tail :accessor node-tail :type node :initarg :tail)))


(defun make-top-level-node (functor arity arguments)
  (make-instance 'top-level-node
                 :functor functor
                 :arity arity
                 :arguments arguments))

(defun make-structure-node (functor arity arguments)
  (make-instance 'structure-node
                 :functor functor
                 :arity arity
                 :arguments arguments))

(defun make-variable-node (variable)
  (make-instance 'variable-node :variable variable))

(defun make-argument-variable-node (variable)
  (make-instance 'argument-variable-node
                 :variable (if (eq variable +wildcard-symbol+)
                             (gensym "?")
                             variable)))

(defun make-list-node (head tail)
  (make-instance 'list-node :head head :tail tail))


(defgeneric node-children (node)
  (:documentation
    "Return the children of the given node.

    Presumably these will need to be traversed when allocating registers."))

(defmethod node-children ((node vanilla-node))
  (list))

(defmethod node-children ((node top-level-node))
  (node-arguments node))

(defmethod node-children ((node structure-node))
  (node-arguments node))

(defmethod node-children ((node list-node))
  (list (node-head node) (node-tail node)))


(defun nil-node-p (node)
  "Return whether the given node is the magic nil/0 constant."
  (and (typep node 'structure-node)
       (eql (node-functor node) nil)
       (zerop (node-arity node))))


(defparameter *dump-node-indent* 0)

(defun print-node-register (node stream &optional space-before)
  (when (slot-boundp node 'register)
    (format stream (if space-before " ~A =" "~A = ") (node-register node))))

(defun print-node-secondary-register (node stream &optional space-before)
  (when (slot-boundp node 'secondary-register)
    (format stream
            (if space-before " ~A =" "~A = ")
            (node-secondary-register node))))

(defgeneric dump-node (node))

(defmethod dump-node ((node node))
  (format t "~VAAN NODE" *dump-node-indent* ""))

(defmethod dump-node ((node variable-node))
  (format t "~VA#<VAR" *dump-node-indent* "")
  (print-node-register node t t)
  (format t " ~S>" (node-variable node)))

(defmethod dump-node ((node argument-variable-node))
  (format t "~VA#<VAR" *dump-node-indent* "")
  (print-node-register node t t)
  (print-node-secondary-register node t t)
  (format t " ~S>" (node-variable node)))

(defmethod dump-node ((node structure-node))
  (format t "~VA#<STRUCT " *dump-node-indent* "")
  (print-node-register node t)
  (format t "~A/~D" (node-functor node) (node-arity node))
  (let ((*dump-node-indent* (+ *dump-node-indent* 4)))
    (dolist (a (node-arguments node))
      (terpri)
      (dump-node a)))
  (format t ">"))

(defmethod dump-node ((node list-node))
  (format t "~VA#<LIST" *dump-node-indent* "")
  (print-node-register node t t)
  (let ((*dump-node-indent* (+ *dump-node-indent* 4)))
    (loop :for element = node :then tail
          :while (typep element 'list-node)
          :for head = (node-head element)
          :for tail = (node-tail element)
          :do (progn (terpri) (dump-node head))
          :finally (when (not (nil-node-p element))
                     (format t "~%~VA.~%" *dump-node-indent* "")
                     (dump-node element))))
  (format t ">"))

(defmethod dump-node ((node top-level-node))
  (format t "#<~A/~D" (node-functor node) (node-arity node))
  (let ((*dump-node-indent* 4))
    (dolist (n (node-arguments node))
      (terpri)
      (dump-node n)))
  (format t ">"))

(defmethod print-object ((node node) stream)
  (let ((*standard-output* stream))
    (dump-node node)))


(defun parse-list (contents)
  (if contents
    (make-list-node (parse (car contents))
                    (parse-list (cdr contents)))
    (make-structure-node 'nil 0 ())))

(defun parse-list* (contents)
  (destructuring-bind (next . remaining) contents
    (if (null remaining)
      (parse next)
      (make-list-node (parse next)
                      (parse-list* remaining)))))

(defun parse (term &optional top-level-argument)
  (cond
    ((variablep term)
     (if top-level-argument
       (make-argument-variable-node term)
       (make-variable-node term)))
    ((symbolp term)
     (parse (list term))) ; c/0 -> (c/0)
    ((consp term)
     (destructuring-bind (functor . arguments) term
       (case functor
         (list (parse-list arguments))
         (list* (parse-list* arguments))
         (t (make-structure-node functor
                                 (length arguments)
                                 (mapcar #'parse arguments))))))))

(defun parse-top-level (term)
  (if (symbolp term) ; c/0 -> (c/0)
    (parse-top-level (list term))
    (destructuring-bind (functor . arguments) term
      (make-top-level-node functor (length arguments)
                           (mapcar (lambda (a) (parse a t))
                                   arguments)))))


;;;; Clause Properties
;;; When tokenizing/precompiling a clause there are a few pieces of metadata
;;; we're going to need.  We group them into a struct to make it easier to pass
;;; everything around.

(defstruct (clause-properties (:conc-name clause-))
  (nead-vars nil :type list)
  (nead-arity 0 :type arity)
  (permanent-vars nil :type list)
  (anonymous-vars nil :type list))


(defun find-variables (terms)
  "Return the set of variables in `terms`."
  (remove-duplicates (tree-collect #'variablep terms)))

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
    (list) ; Facts and chain rules have no permanent variables at all
    (destructuring-bind (head body-first . body-rest) clause
      ;; The head is treated as part of the first goal for the purposes of
      ;; finding permanent variables.
      (find-shared-variables (cons (cons head body-first) body-rest)))))

(defun find-nead-variables (clause)
  "Return a list of all variables in the nead of `clause`.

  The head and neck (first term in the body) are the 'nead'.

  "
  (if (<= (length clause) 1)
    (list)
    (destructuring-bind (head body-first . body-rest) clause
      (declare (ignore body-rest))
      (find-variables (list head body-first)))))

(defun find-anonymous-variables (clause)
  "Return a list of all anonymous variables in `clause`.

  Anonymous variables are non-argument-position variables that are only ever
  used once.

  "
  ;; clause: ((member :x (list* :y :rest)) ;-
  ;;          (member :x :rest))
  ;;
  ;; terms: (member :x (list* :y :rest))
  ;;        (member :x :rest)
  (flet ((argument-variables (term)
           (remove-if-not #'variablep (cdr term)))
         (non-argument-variables (term)
           (loop :for argument :in (cdr term)
                 :when (consp argument)
                 :append (tree-collect #'variablep (cdr argument)))))
    (let ((terms (remove-if-not #'consp clause)))
      (set-difference (unique-items (mapcan #'non-argument-variables terms))
                      (mapcan #'argument-variables terms)))))


(defun determine-clause-properties (head body)
  (let* ((clause
           (cons head body))
         (permanent-vars
           (if (null head)
             ;; For query clauses we cheat a bit and make ALL variables
             ;; permanent, so we can extract their bindings as results later.
             (find-variables body)
             (find-permanent-variables clause)))
         (anonymous-vars
           (if (null head)
             ;; Again, for queries we cheat and never let anything be
             ;; anonymous (except for the wildcard).
             (list +wildcard-symbol+)
             (cons +wildcard-symbol+
                   (find-anonymous-variables clause))))
         (nead-vars
           (set-difference (find-nead-variables clause)
                           permanent-vars))
         (nead-arity
           (max (1- (length head))
                (1- (length (first (remove '! body))))))) ; gross
    (make-clause-properties :nead-vars nead-vars
                            :nead-arity nead-arity
                            :permanent-vars permanent-vars
                            :anonymous-vars anonymous-vars)))


;;;; Register Allocation
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
  local-registers
  stack-registers
  permanent-variables
  anonymous-variables
  reserved-variables
  reserved-arity
  actual-arity)


(defun find-variable (state variable)
  "Return the register that already contains this variable, or `nil` otherwise."
  (or (when-let (r (position variable (allocation-state-local-registers state)))
        (make-temporary-register r (allocation-state-actual-arity state)))
      (when-let (s (position variable (allocation-state-stack-registers state)))
        (make-permanent-register s))
      nil))

(defun store-variable (state variable)
  "Assign `variable` to the next available local register.

  It is assumed that `variable` is not already assigned to another register
  (check that with `find-variable` first).

  It is also assumed that this will be a non-argument register, because as
  mentioned above variables cannot live directly inside argument registers.

  "
  (make-register
    :local
    (vector-push-extend variable (allocation-state-local-registers state))))

(defun ensure-variable (state variable)
  (or (find-variable state variable)
      (store-variable state variable)))


(defmacro set-when-unbound (instance slot value-form)
  (once-only (instance slot)
    `(when (not (slot-boundp ,instance ,slot))
       (setf (slot-value ,instance ,slot) ,value-form))))


(defun allocate-nonvariable-register (state)
  "Allocate and return a register for something that's not a variable."
  ;; We need to allocate registers for things like structures and lists, but we
  ;; never need to look them up later (like we do with variables), so we'll just
  ;; shove a nil into the local registers array as a placeholder.
  (make-temporary-register
    (vector-push-extend nil (allocation-state-local-registers state))
    (allocation-state-actual-arity state)))


(defgeneric allocate-register (node allocation-state))


(defmethod allocate-register ((node top-level-node) state)
  (declare (ignore node state))
  (values))

(defmethod allocate-register ((node variable-node) state)
  (set-when-unbound node 'register
    (if (member (node-variable node)
                (allocation-state-anonymous-variables state))
      (make-anonymous-register)
      (ensure-variable state (node-variable node)))))

(defmethod allocate-register ((node argument-variable-node) state)
  (set-when-unbound node 'secondary-register
    (ensure-variable state (node-variable node))))

(defmethod allocate-register ((node structure-node) state)
  (set-when-unbound node 'register
    (allocate-nonvariable-register state)))

(defmethod allocate-register ((node list-node) state)
  (set-when-unbound node 'register
    (allocate-nonvariable-register state)))


(defun allocate-argument-registers (node)
  (loop :for argument :in (node-arguments node)
        :for i :from 0
        :do (setf (node-register argument)
                  (make-register :argument i)))
  (values))

(defun allocate-nonargument-registers (node clause-props &key nead)
  ;; JESUS TAKE THE WHEEL
  (let*
      ((actual-arity (node-arity node))
       (reserved-arity (when nead
                         (clause-nead-arity clause-props)))
       (reserved-variables (when nead
                             (clause-nead-vars clause-props)))
       (permanent-variables (clause-permanent-vars clause-props))
       ;; Preallocate enough registers for all of the arguments.  We'll fill
       ;; them in later.  Note that things are more complicated in the head and
       ;; first body term of a clause (see above).
       (local-registers (make-array 64
                          :fill-pointer (or reserved-arity actual-arity)
                          :adjustable t
                          :initial-element nil))
       ;; We essentially "preallocate" all the permanent variables up front
       ;; because we need them to always be in the same stack registers across
       ;; all the terms of our clause.
       ;;
       ;; The ones that won't get used in this term will end up getting
       ;; flattened away anyway.
       (stack-registers (make-array (length permanent-variables)
                          :initial-contents permanent-variables))
       (allocation-state
         (make-allocation-state
           :local-registers local-registers
           :stack-registers stack-registers
           :permanent-variables permanent-variables
           :anonymous-variables (clause-anonymous-vars clause-props)
           :reserved-variables reserved-variables
           :reserved-arity reserved-arity
           :actual-arity actual-arity)))
    ;; Actually reserve the reserved (but non-permanent, see above) variables.
    ;; They need to live in consistent spots for the head and first body term.
    (loop :for variable :in reserved-variables
          :do (vector-push-extend variable local-registers))
    (recursively ((remaining (list node)))
      (when remaining
        (destructuring-bind (node . remaining) remaining
          (allocate-register node allocation-state)
          (recur (append remaining (node-children node)))))))
  (values))

(defun allocate-registers (node clause-props &key nead)
  (allocate-argument-registers node)
  (allocate-nonargument-registers node clause-props :nead nead)
  (values))


;;;; Flattening
;;; "Flattening" is the process of turning a parse tree (with register
;;; assignments) into a flat list of nodes, which will then be turned into
;;; a series of instructions.
;;;
;;; The order of this list depends on whether we're compiling a query term or
;;; a program term.
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
;;;   X2 <- q(X1, X3)
;;;   X0 <- p(X1, X2)

(defclass register-assignment ()
  ((register :accessor assignment-register :type register :initarg :register)))


(defclass structure-assignment (register-assignment)
  ((functor :accessor assignment-functor :type symbol :initarg :functor)
   (arity :accessor assignment-arity :type arity :initarg :arity)
   (arguments :accessor assignment-arguments :type list :initarg :arguments)))

(defclass argument-variable-assignment (register-assignment)
  ((target :accessor assignment-target :type register :initarg :target)))

(defclass list-assignment (register-assignment)
  ((head :accessor assignment-head :type register :initarg :head)
   (tail :accessor assignment-tail :type register :initarg :tail)))


(defmethod print-object ((assignment structure-assignment) stream)
  (print-unreadable-object (assignment stream :type nil :identity nil)
    (format stream "~A = ~A/~D(~{~A~^, ~})"
            (register-to-string (assignment-register assignment))
            (assignment-functor assignment)
            (assignment-arity assignment)
            (mapcar #'register-to-string (assignment-arguments assignment)))))

(defmethod print-object ((assignment argument-variable-assignment) stream)
  (print-unreadable-object (assignment stream :type nil :identity nil)
    (format stream "~A = ~A"
            (register-to-string (assignment-register assignment))
            (register-to-string (assignment-target assignment)))))

(defmethod print-object ((assignment list-assignment) stream)
  (print-unreadable-object (assignment stream :type nil :identity nil)
    (format stream "~A = [~A | ~A]"
            (register-to-string (assignment-register assignment))
            (register-to-string (assignment-head assignment))
            (register-to-string (assignment-tail assignment)))))


(defgeneric node-flatten (node))

(defmethod node-flatten (node)
  nil)

(defmethod node-flatten ((node structure-node))
  (make-instance 'structure-assignment
                 :register (node-register node)
                 :functor (node-functor node)
                 :arity (node-arity node)
                 :arguments (mapcar #'node-register (node-arguments node))))

(defmethod node-flatten ((node argument-variable-node))
  (make-instance 'argument-variable-assignment
                 :register (node-register node)
                 :target (node-secondary-register node)))

(defmethod node-flatten ((node list-node))
  (make-instance 'list-assignment
                 :register (node-register node)
                 :head (node-register (node-head node))
                 :tail (node-register (node-tail node))))


(defun flatten-breadth-first (tree)
  (let ((results nil))
    (recursively ((node tree))
      (when-let (assignment (node-flatten node))
        (push assignment results))
      (mapcar #'recur (node-children node)))
    (nreverse results)))

(defun flatten-depth-first-post-order (tree)
  (let ((results nil))
    (recursively ((node tree))
      (mapcar #'recur (node-children node))
      (when-let (assignment (node-flatten node))
        (push assignment results)))
    (nreverse results)))


(defun flatten-query (tree)
  (flatten-depth-first-post-order tree))

(defun flatten-program (tree)
  (flatten-breadth-first tree))


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

(defclass token () ())


(defclass register-token (token)
  ((register :accessor token-register :type register :initarg :register)))

(defclass structure-token (register-token)
  ((functor :accessor token-functor :type symbol :initarg :functor)
   (arity :accessor token-arity :type arity :initarg :arity)))

(defclass argument-variable-token (register-token)
  ((target :accessor token-target :type register :initarg :target)))

(defclass list-token (register-token) ())

(defclass call-token (token)
  ((functor :accessor token-functor :type symbol :initarg :functor)
   (arity :accessor token-arity :type arity :initarg :arity)))

(defclass cut-token (token) ())


(defun make-register-token (register)
  (make-instance 'register-token :register register))


(defmethod print-object ((token register-token) stream)
  (print-object (token-register token) stream))

(defmethod print-object ((token structure-token) stream)
  (print-unreadable-object (token stream :identity nil :type nil)
    (format stream "~A = ~A/~D"
            (register-to-string (token-register token))
            (token-functor token)
            (token-arity token))))

(defmethod print-object ((token argument-variable-token) stream)
  (print-unreadable-object (token stream :identity nil :type nil)
    (format stream "~A = ~A"
            (register-to-string (token-register token))
            (register-to-string (token-target token)))))

(defmethod print-object ((token list-token) stream)
  (print-unreadable-object (token stream :identity nil :type nil)
    (format stream "~A = LIST" (register-to-string (token-register token)))))

(defmethod print-object ((token call-token) stream)
  (print-unreadable-object (token stream :identity nil :type nil)
    (format stream "CALL ~A/~D"
            (token-functor token)
            (token-arity token))))

(defmethod print-object ((token cut-token) stream)
  (print-unreadable-object (token stream :identity nil :type nil)
    (format stream "CUT!")))


(defgeneric tokenize-assignment (assignment))

(defmethod tokenize-assignment ((assignment structure-assignment))
  (list* (make-instance 'structure-token
                        :register (assignment-register assignment)
                        :functor (assignment-functor assignment)
                        :arity (assignment-arity assignment))
         (mapcar #'make-register-token (assignment-arguments assignment))))

(defmethod tokenize-assignment ((assignment argument-variable-assignment))
  (list (make-instance 'argument-variable-token
                       :register (assignment-register assignment)
                       :target (assignment-target assignment))))

(defmethod tokenize-assignment ((assignment list-assignment))
  (list (make-instance 'list-token :register (assignment-register assignment))
        (make-register-token (assignment-head assignment))
        (make-register-token (assignment-tail assignment))))


(defun tokenize-assignments (assignments)
  "Tokenize a flattened set of register assignments into a stream."
  (mapcan #'tokenize-assignment assignments))


(defun tokenize-program-term (term clause-props)
  "Tokenize `term` as a program term, returning its tokens."
  (let ((tree (parse-top-level term)))
    (allocate-registers tree clause-props :nead t)
    (-> tree flatten-program tokenize-assignments)))

(defun tokenize-query-term (term clause-props &key nead)
  "Tokenize `term` as a query term, returning its tokens."
  (let ((tree (parse-top-level term)))
    (allocate-registers tree clause-props :nead nead)
    (-<> tree
      flatten-query
      tokenize-assignments
      ;; We need to shove a CALL token onto the end.
      (append <> (list (make-instance 'call-token
                                      :functor (node-functor tree)
                                      :arity (node-arity tree)))))))


;;;; Precompilation
;;; Once we have a tokenized stream we can generate the machine instructions
;;; from it.
;;;
;;; We don't generate the ACTUAL bytecode immediately, because we want to run
;;; a few optimization passes on it first, and it's easier to work with if we
;;; have a friendlier format.
;;;
;;; So we turn a stream of tokens:
;;;
;;;   (X2 = q/2), X1, X3, (X0 = p/2), X1, X2
;;;
;;; into a list of instructions, each of which is a list:
;;;
;;;   (:put-structure X2 q 2)
;;;   (:set-variable X1)
;;;   (:set-variable X3)
;;;   (:put-structure X0 p 2)
;;;   (:set-value X1)
;;;   (:set-value X2)
;;;
;;; The opcodes are keywords and the register arguments remain register objects.
;;; They get converted down to the raw bytes in the final "rendering" step.
;;;
;;; # Cut
;;;
;;; A quick note on cut (!): the book and original WAM do some nutty things to
;;; save one stack word per frame.  They store the cut register for non-neck
;;; cuts in a "pseudovariable" on the stack, so they only have to allocate that
;;; extra stack word for things that actually USE non-neck cuts.
;;;
;;; We're going to just eat the extra stack word and store the cut register in
;;; every frame instead.  This massively simplifies the implementation and lets
;;; me keep my sanity, and it MIGHT even end up being faster because there's
;;; one fewer opcode, less fucking around in the compiler, etc.  But regardless:
;;; I don't want to go insane, and my laptop has sixteen gigabytes of RAM, so
;;; let's just store the damn word.
;;;
;;; # "Seen" Registers
;;;
;;; The book neglects to mention some REALLY important information about how you
;;; have to handle registers when compiling a stream of tokens.  But if you've
;;; made it this far, you should be pretty used to the book omitting vital
;;; information.  So hop in the clown car and take a ride with me.
;;;
;;; From the very beginning,the book mentions that certain instructions come in
;;; pairs, the first of which is used the first time the register is "seen" or
;;; "encountered", and the second used of which is used subsequent times.
;;;
;;; For example, a simple query like `p(A, A, A)` would result in:
;;;
;;;     put-variable A0 X3
;;;     put-value A1 X3
;;;     put-value A2 X3
;;;     call p/3
;;;
;;; This is all fine and dandy and works for single goals, but if you have
;;; a clause with MULTIPLE body goals you need to "reset" the list of
;;; already-seen registers after each goal.  For example, consider:
;;;
;;;     p() :-
;;;       f(X, X),
;;;       g(Y, Y).
;;;
;;; If you just apply what the book says without resetting the already-seen
;;; register list, you get:
;;;
;;;     put-variable A0 X2
;;;     put-value A1 X2
;;;     call f/2
;;;     put-value A0 X2   <--- wrong!
;;;     put-value A1 X2
;;;     call g/2
;;;
;;; But the variable in `g/2` is DIFFERENT than the one used in `f/2`, so that
;;; second `put-value` instruction is wrong!  What we need instead is this:
;;;
;;;     put-variable A0 X2
;;;     put-value A1 X2
;;;     call f/2
;;;     put-variable A0 X2   <--- right!
;;;     put-value A1 X2
;;;     call g/2
;;;
;;; So the list of seen registers needs to get cleared after each body goal.
;;;
;;; But be careful: it's only TEMPORARY registers that need to get cleared!  If
;;; the variables in our example WEREN'T different (`p() :- f(X, X), g(X, X)`)
;;; the instructions would be assigning to stack registers, and we WANT to do
;;; one `put-variable` and have the rest be `put-value`s.
;;;
;;; And there's one more edge case you're probably wondering about: what happens
;;; after the HEAD of a clause?  Do we need to reset?  The answer is: no,
;;; because the head and first body goal share registers, which is what performs
;;; the "substitution" for the first body goal (see the comment earlier for more
;;; on that rabbit hole).

(defun find-opcode (opcode first-seen mode &optional register)
  (let ((register-variant (when register
                            (case (register-type register)
                              ((:local :argument) :local)
                              ((:permanent) :stack)
                              ((:anonymous) :void)))))
    (case opcode ; oh fuck off
      (:argument (if first-seen
                   (case mode
                     (:program (case register-variant
                                 (:local :get-variable-local)
                                 (:stack :get-variable-stack)))
                     (:query (case register-variant
                               (:local :put-variable-local)
                               (:stack :put-variable-stack))))
                   (case mode
                     (:program (case register-variant
                                 (:local :get-value-local)
                                 (:stack :get-value-stack)))
                     (:query (case register-variant
                               (:local :put-value-local)
                               (:stack :put-value-stack))))))
      ;; Structures and lists can only live locally, they never go on the stack
      (:structure (case mode
                    (:program :get-structure)
                    (:query :put-structure)))
      (:list (case mode
               (:program :get-list)
               (:query :put-list)))
      (:register (if first-seen
                   (case mode
                     (:program (case register-variant
                                 (:local :unify-variable-local)
                                 (:stack :unify-variable-stack)
                                 (:void :unify-void)))
                     (:query (case register-variant
                               (:local :set-variable-local)
                               (:stack :set-variable-stack)
                               (:void :set-void))))
                   (case mode
                     (:program (case register-variant
                                 (:local :unify-value-local)
                                 (:stack :unify-value-stack)
                                 (:void :unify-void)))
                     (:query (case register-variant
                               (:local :set-value-local)
                               (:stack :set-value-stack)
                               (:void :set-void)))))))))


(defun precompile-tokens (wam head-tokens body-tokens)
  "Generate a series of machine instructions from a stream of head and body
  tokens.

  The `head-tokens` should be program-style tokens, and are compiled in program
  mode.  The `body-tokens` should be query-style tokens, and are compiled in
  query mode.

  Actual queries are a special case where the `head-tokens` stream is `nil`

  The compiled instructions will be returned as a circle.

  "
  (let ((seen (list))
        (mode nil)
        (instructions (make-empty-circle)))
    (labels
        ((push-instruction (&rest instruction)
           (circle-insert-end instructions instruction))
         (reset-seen ()
           ;; Reset the list of seen registers (grep for "clown car" above)
           (setf seen (remove-if #'register-temporary-p seen)))
         (handle-argument (argument-register source-register)
           ;; OP X_n A_i
           (let ((newp (push-if-new source-register seen :test #'register=)))
             (push-instruction (find-opcode :argument newp mode source-register)
                               source-register
                               argument-register)))
         (handle-structure (destination-register functor arity)
           ;; OP functor reg
           (push destination-register seen)
           (push-instruction (find-opcode :structure nil mode destination-register)
                             (wam-ensure-functor-index wam (cons functor arity))
                             destination-register))
         (handle-list (register)
           (push register seen)
           (push-instruction (find-opcode :list nil mode register)
                             register))
         (handle-cut ()
           (push-instruction :cut))
         (handle-call (functor arity)
           (if (and (eq functor 'call)
                    (= arity 1))
             ;; DYNAMIC-CALL
             (push-instruction :dynamic-call)
             ;; CALL functor
             (push-instruction
               :call
               (wam-ensure-functor-index wam (cons functor arity))))
           ;; This is a little janky, but at this point the body goals have been
           ;; turned into one single stream of tokens, so we don't have a nice
           ;; clean way to tell when one ends.  But in practice, a body goal is
           ;; going to end with a CALL instruction, so we can use this as
           ;; a kludge to know when to reset.
           ;;
           ;; TODO: We should probably dekludge this by emitting an extra "end
           ;; body goal" token, especially once we add some special forms that
           ;; might need to do some resetting but not end in a CALL.
           (reset-seen))
         (handle-register (register)
           ;; OP reg
           (if (eq (register-type register) :anonymous)
             (push-instruction (find-opcode :register nil mode register) 1)
             (let ((first-seen (push-if-new register seen :test #'register=)))
               (push-instruction
                 (find-opcode :register first-seen mode register)
                 register))))
         (handle-token (token)
           (etypecase token
             (argument-variable-token
               (handle-argument (token-register token)
                                (token-target token)))
             (structure-token
               (handle-structure (token-register token)
                                 (token-functor token)
                                 (token-arity token)))
             (list-token
               (handle-list (token-register token)))
             (cut-token
               (handle-cut))
             (call-token
               (handle-call (token-functor token)
                            (token-arity token)))
             (register-token
               (handle-register (token-register token)))))
         (handle-stream (tokens)
           (map nil #'handle-token tokens)))
      (when head-tokens
        (setf mode :program)
        (handle-stream head-tokens))
      (setf mode :query)
      (handle-stream body-tokens)
      instructions)))


(defun precompile-clause (wam head body)
  "Precompile the clause.

  `head` should be the head of the clause for program clauses, or `nil` for
  query clauses.

  `body` is the body of the clause, or `nil` for facts.

  Returns a circle of instructions and the properties of the clause.

  "
  (let* ((clause-props
           (determine-clause-properties head body))
         (head-tokens
           (when head
             (tokenize-program-term head clause-props)))
         (body-tokens
           (when body
             (loop
               :with first = t
               :for goal :in body
               :append
               (cond
                 ;; cut just gets emitted straight, but DOESN'T flip `first`...
                 ;; TODO: fix the cut layering violation here...
                 ((eql goal '!) ; gross
                  (list (make-instance 'cut-token)))
                 (first
                  (setf first nil)
                  (tokenize-query-term goal clause-props
                                       :nead t))
                 (t
                  (tokenize-query-term goal clause-props)))))))
    (let ((instructions (precompile-tokens wam head-tokens body-tokens))
          (variable-count (length (clause-permanent-vars clause-props))))
      ;; We need to compile facts and rules differently.  Facts end with
      ;; a PROCEED and rules are wrapped in ALOC/DEAL.
      (cond
        ((and head body) ; a full-ass rule
         (circle-insert-beginning instructions `(:allocate ,variable-count))
         (circle-insert-end instructions `(:deallocate)))

        ((and head (null body)) ; a bare fact
         (circle-insert-end instructions `(:proceed)))

        (t ; a query
         ;; The book doesn't have this ALOC here, but we do it to aid in result
         ;; extraction.  Basically, to make extracting th results of a query
         ;; easier we allocate all of its variables on the stack, so we need
         ;; push a stack frame for them before we get started.  We don't DEAL
         ;; because we want the frame to be left on the stack at the end so we
         ;; can poke at it.
         (circle-insert-beginning instructions `(:allocate ,variable-count))
         (circle-insert-end instructions `(:done))))
      (values instructions clause-props))))


(defun precompile-query (wam query)
  "Compile `query`, returning the instructions and permanent variables.

  `query` should be a list of goal terms.

  "
  (multiple-value-bind (instructions clause-props)
      (precompile-clause wam nil query)
    (values instructions
            (clause-permanent-vars clause-props))))


(defun find-predicate (clause)
  "Return a pair of the functor and arity of `clause`

  A functor and an arity together specify a particular Prolog predicate.

  "
  ;; ( (f ?x ?y)   | head   ||| clause
  ;;   (foo ?x)    || body  |||
  ;;   (bar ?y) )  ||       |||
  (destructuring-bind (head . body) clause
    (declare (ignore body))
    (cond
      ((null head)
       (error "Clause ~S has a NIL head." clause))
      ((atom head) ; constants are 0-arity
       (cons head 0))
      (t
       (cons (car head)
             (1- (length head)))))))

(defun check-rules (rules)
  (let ((predicates (-<> rules
                      (mapcar #'find-predicate <>)
                      (remove-duplicates <> :test #'equal))))
    (assert (= 1 (length predicates)) ()
      "Must add exactly one predicate at a time (got: ~S)."
      predicates)
    (values (car (first predicates))
            (cdr (first predicates)))))

(defun precompile-rules (wam rules)
  "Compile `rules` into a list of instructions.

  Each rule in `rules` should be a clause consisting of a head term and zero or
  more body terms.  A rule with no body is called a fact.

  Returns the circle of compiled instructions, as well as the functor and arity
  of the rules being compiled.

  "
  (assert rules () "Cannot compile an empty program.")
  (multiple-value-bind (functor arity) (check-rules rules)
    (values
      (if (= 1 (length rules))
        ;; Single-clause rules don't need to bother setting up a choice point.
        (destructuring-bind ((head . body)) rules
          (precompile-clause wam head body))
        ;; Otherwise we need to loop through each of the clauses, pushing their
        ;; choice point instruction first, then their actual code.
        ;;
        ;; The `nil` clause addresses will get filled in later, during rendering.
        (loop :with instructions = (make-empty-circle)
              :for ((head . body) . remaining) :on rules
              :for first-p = t :then nil
              :for last-p = (null remaining)
              :for clause-instructions = (precompile-clause wam head body)
              :do (progn
                    (circle-insert-end instructions
                                       (cond (first-p '(:try nil))
                                             (last-p '(:trust))
                                             (t '(:retry nil))))
                    (circle-append-circle instructions clause-instructions))
              :finally (return instructions)))
      functor
      arity)))


;;;; Optimization
;;; Optimization of the WAM instructions happens between the precompilation
;;; phase and the rendering phase.  We perform a number of passes over the
;;; circle of instructions, doing one optimization each time.

(defun optimize-get-constant (node constant register)
  ;; 1. get_structure c/0, Ai -> get_constant c, Ai
  (circle-replace node `(:get-constant ,constant ,register)))

(defun optimize-put-constant (node constant register)
  ;; 2. put_structure c/0, Ai -> put_constant c, Ai
  (circle-replace node `(:put-constant ,constant ,register)))

(defun optimize-set-constant (node constant register)
  ;; 3. put_structure c/0, Xi                     *** WE ARE HERE
  ;;    ...
  ;;    set_value Xi          -> set_constant c
  (loop
    :with previous = (circle-prev node)
    ;; Search forward for the corresponding set-value instruction
    :for n = (circle-forward-remove node) :then (circle-forward n)
    :while n
    :for (opcode . arguments) = (circle-value n)
    :when (and (eql opcode :set-value-local)
               (register= register (first arguments)))
    :do
    (circle-replace n `(:set-constant ,constant))
    (return previous)))

(defun optimize-unify-constant (node constant register)
  ;; 4. unify_variable Xi     -> unify_constant c
  ;;    ...
  ;;    get_structure c/0, Xi                     *** WE ARE HERE
  (loop
    ;; Search backward for the corresponding unify-variable instruction
    :for n = (circle-backward node) :then (circle-backward n)
    :while n
    :for (opcode . arguments) = (circle-value n)
    :when (and (eql opcode :unify-variable-local)
               (register= register (first arguments)))
    :do
    (circle-replace n `(:unify-constant ,constant))
    (return (circle-backward-remove node))))

(defun optimize-constants (wam instructions)
  ;; From the book and the erratum, there are four optimizations we can do for
  ;; constants (0-arity structures).
  (flet ((constant-p (functor)
           (zerop (wam-functor-arity wam functor))))
    (loop :for node = (circle-forward instructions) :then (circle-forward node)
          :while node
          :for (opcode . arguments) = (circle-value node)
          :do
          (match (circle-value node)

            ((guard `(:put-structure ,functor ,register)
                    (constant-p functor))
             (setf node
                   (if (register-argument-p register)
                     (optimize-put-constant node functor register)
                     (optimize-set-constant node functor register))))

            ((guard `(:get-structure ,functor ,register)
                    (constant-p functor))
             (setf node
                   (if (register-argument-p register)
                     (optimize-get-constant node functor register)
                     (optimize-unify-constant node functor register))))))
    instructions))


(defun optimize-void-runs (wam instructions)
  ;; We can optimize runs of N (:[unify/set]-void 1) instructions into a single
  ;; one that does all N at once.
  (declare (ignore wam))
  (loop
    :for node = (circle-forward instructions) :then (circle-forward node)
    :while node
    :for opcode = (car (circle-value node))
    :when (or (eq opcode :set-void)
              (eq opcode :unify-void))
    :do
    (loop
      :with beginning = (circle-backward node)
      :for run-node = node :then (circle-forward run-node)
      :for run-opcode = (car (circle-value run-node))
      :while (eq opcode run-opcode)
      :do (circle-remove run-node)
      :sum 1 :into run-length
      :finally
      (progn
        (setf node (circle-forward beginning))
        (circle-insert-after beginning
                             `(,opcode ,run-length))))))


(defun optimize-instructions (wam instructions)
  (->> instructions
    (optimize-constants wam)
    (optimize-void-runs wam)))


;;;; Rendering
;;; Rendering is the act of taking the friendly list-of-instructions format and
;;; actually converting it to raw-ass bytes and storing it in an array.

(defun render-opcode (opcode)
  (ecase opcode
    (:get-structure        +opcode-get-structure+)
    (:unify-variable-local +opcode-unify-variable-local+)
    (:unify-variable-stack +opcode-unify-variable-stack+)
    (:unify-value-local    +opcode-unify-value-local+)
    (:unify-value-stack    +opcode-unify-value-stack+)
    (:unify-void           +opcode-unify-void+)
    (:get-variable-local   +opcode-get-variable-local+)
    (:get-variable-stack   +opcode-get-variable-stack+)
    (:get-value-local      +opcode-get-value-local+)
    (:get-value-stack      +opcode-get-value-stack+)
    (:put-structure        +opcode-put-structure+)
    (:set-variable-local   +opcode-set-variable-local+)
    (:set-variable-stack   +opcode-set-variable-stack+)
    (:set-value-local      +opcode-set-value-local+)
    (:set-value-stack      +opcode-set-value-stack+)
    (:set-void             +opcode-set-void+)
    (:put-variable-local   +opcode-put-variable-local+)
    (:put-variable-stack   +opcode-put-variable-stack+)
    (:put-value-local      +opcode-put-value-local+)
    (:put-value-stack      +opcode-put-value-stack+)
    (:put-constant         +opcode-put-constant+)
    (:get-constant         +opcode-get-constant+)
    (:set-constant         +opcode-set-constant+)
    (:get-list             +opcode-get-list+)
    (:put-list             +opcode-put-list+)
    (:unify-constant       +opcode-unify-constant+)
    (:call                 +opcode-call+)
    (:dynamic-call         +opcode-dynamic-call+)
    (:proceed              +opcode-proceed+)
    (:allocate             +opcode-allocate+)
    (:deallocate           +opcode-deallocate+)
    (:done                 +opcode-done+)
    (:try                  +opcode-try+)
    (:retry                +opcode-retry+)
    (:trust                +opcode-trust+)
    (:cut                  +opcode-cut+)))

(defun render-argument (argument)
  (etypecase argument
    (null 0) ; ugly choice point args that'll be filled later...
    (register (register-number argument)) ; bytecode just needs register numbers
    (number argument))) ; just a numeric argument, e.g. alloc 0

(defun render-bytecode (code instructions)
  "Render `instructions` (a circle) into `code` (a bytecode array)."
  (let ((previous-jump nil))
    (flet
        ((fill-previous-jump (address)
           (when previous-jump
             (setf (aref code (1+ previous-jump)) address))
           (setf previous-jump address)))
      (loop
        :for (opcode . arguments) :in (circle-to-list instructions)
        :for address = (code-push-instruction! code
                           (render-opcode opcode)
                         (mapcar #'render-argument arguments))
        ;; We need to fill in the addresses for the choice point jumping
        ;; instructions.  For example, when we have TRY ... TRUST, the TRUST
        ;; needs to patch its address into the TRY instruction.
        ;;
        ;; I know, this is ugly, sorry.
        :when (member opcode '(:try :retry :trust))
        :do (fill-previous-jump address)))))


(defun make-query-code-store ()
  (make-array 512
    :fill-pointer 0
    :adjustable t
    :element-type 'code-word))

(defun render-query (instructions)
  (let ((code (make-query-code-store)))
    (render-bytecode code instructions)
    code))


(defun mark-label (wam functor arity address)
  "Set the code label `functor`/`arity` to point at `address`."
  (setf (wam-code-label wam functor arity) address))

(defun render-rules (wam functor arity instructions)
  ;; Before we render the instructions, make the label point at where they're
  ;; about to go.
  (mark-label wam functor arity (fill-pointer (wam-code wam)))
  (render-bytecode (wam-code wam) instructions))


;;;; Compilation
;;; The compilation phase wraps everything else up into a sane UI.
(defun compile-query (wam query)
  "Compile `query` into a fresh array of bytecode.

  `query` should be a list of goal terms.

  Returns the fresh code array and the permanent variables.

  "
  (multiple-value-bind (instructions permanent-variables)
      (precompile-query wam query)
    (optimize-instructions wam instructions)
    (values
      (render-query instructions)
      permanent-variables)))

(defun compile-rules (wam rules)
  "Compile `rules` into the WAM's code store.

  Each rule in `rules` should be a clause consisting of a head term and zero or
  more body terms.  A rule with no body is called a fact.

  "
  (multiple-value-bind (instructions functor arity)
      (precompile-rules wam rules)
    (optimize-instructions wam instructions)
    (render-rules wam functor arity instructions)))
