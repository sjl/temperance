(in-package #:bones.wam)

;;;; .-,--.                             .      .
;;;;  '|__/ ,-. ,-. ,-. ,-. ,-,-. ,-. . |  ,-. |- . ,-. ,-.
;;;;  ,|    |   |-' |   | | | | | | | | |  ,-| |  | | | | |
;;;;  `'    '   `-' `-' `-' ' ' ' |-' ' `' `-^ `' ' `-' ' '
;;;;                              |
;;;;                              '

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
;;;   (:subterm-variable X1)
;;;   (:subterm-variable X3)
;;;   (:put-structure X0 p 2)
;;;   (:subterm-value X1)
;;;   (:subterm-value X2)
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


(defun* find-opcode-register ((first-seen boolean) (register register))
  (:returns keyword)
  (let ((register-variant (when register
                            (ecase (register-type register)
                              ((:local :argument) :local)
                              ((:permanent) :stack)
                              ((:anonymous) :void)))))
    (if first-seen
      (ecase register-variant
        (:local :subterm-variable-local)
        (:stack :subterm-variable-stack)
        (:void :subterm-void))
      (ecase register-variant
        (:local :subterm-value-local)
        (:stack :subterm-value-stack)
        (:void :subterm-void)))))

(defun* find-opcode-list ((mode keyword))
  (:returns keyword)
  (ecase mode
    (:program :get-list)
    (:query :put-list)))

(defun* find-opcode-lisp-object ((mode keyword))
  (:returns keyword)
  (ecase mode
    (:program :get-lisp-object)
    (:query :put-lisp-object)))

(defun* find-opcode-structure ((mode keyword))
  (:returns keyword)
  (ecase mode
    (:program :get-structure)
    (:query :put-structure)))

(defun* find-opcode-argument ((first-seen boolean)
                              (mode keyword)
                              (register register))
  (:returns keyword)
  (let ((register-variant (ecase (register-type register)
                            ((:local :argument) :local)
                            ((:permanent) :stack))))
    (if first-seen
      (ecase mode
        (:program (ecase register-variant
                    (:local :get-variable-local)
                    (:stack :get-variable-stack)))
        (:query (ecase register-variant
                  (:local :put-variable-local)
                  (:stack :put-variable-stack))))
      (ecase mode
        (:program (ecase register-variant
                    (:local :get-value-local)
                    (:stack :get-value-stack)))
        (:query (ecase register-variant
                  (:local :put-value-local)
                  (:stack :put-value-stack)))))))


(defun* precompile-tokens ((wam wam) (head-tokens list) (body-tokens list))
  (:returns circle)
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
           (if (register-anonymous-p source-register)
             ;; Crazy, but we can just drop argument-position anonymous
             ;; variables on the floor at this point.
             nil
             ;; OP X_n A_i
             (let ((first-seen (push-if-new source-register seen :test #'register=)))
               (push-instruction
                 (find-opcode-argument first-seen mode source-register)
                 source-register
                 argument-register))))
         (handle-structure (destination-register functor arity)
           ;; OP functor reg
           (push destination-register seen)
           (push-instruction (find-opcode-structure mode)
                             (wam-unique-functor wam (cons functor arity))
                             destination-register))
         (handle-list (register)
           (push register seen)
           (push-instruction (find-opcode-list mode)
                             register))
         (handle-lisp-object (register object)
           ;; OP object register
           (push register seen)
           (push-instruction (find-opcode-lisp-object mode) object register))
         (handle-cut ()
           (push-instruction :cut))
         (handle-procedure-call (functor arity is-jump)
           (if (and (eq functor 'call)
                    (= arity 1))
             ;; DYNAMIC-[CALL/JUMP]
             (push-instruction (if is-jump :dynamic-jump :dynamic-call))
             ;; [CALL/JUMP] functor
             (push-instruction
               (if is-jump :jump :call)
               (wam-unique-functor wam (cons functor arity))))
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
           (if (register-anonymous-p register)
             ;; VOID 1
             (push-instruction (find-opcode-register nil register) 1)
             ;; OP reg
             (let ((first-seen (push-if-new register seen :test #'register=)))
               (push-instruction
                 (find-opcode-register first-seen register)
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
             (lisp-object-token
               (handle-lisp-object (token-register token)
                                   (token-object token)))
             (cut-token
               (handle-cut))
             (jump-token
               (handle-procedure-call (token-functor token)
                                      (token-arity token)
                                      t))
             (call-token
               (handle-procedure-call (token-functor token)
                                      (token-arity token)
                                      nil))
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


(defun* precompile-clause ((wam wam) head body)
  (:returns (values circle clause-properties))
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
         (clause-type
           (cond ((null head) :query)
                 ((null body) :fact)
                 ((null (rest body)) :chain)
                 (t :rule)))
         (body-tokens
           (when body
             (loop
               :with first = t
               :for (goal . remaining) :on body
               :append
               (if (eq goal '!) ; gross
                 ;; cut just gets emitted straight, but DOESN'T flip `first`...
                 ;; TODO: fix the cut layering violation here...
                 (list (make-instance 'cut-token))
                 (prog1
                     (tokenize-query-term
                       goal clause-props
                       :in-nead first
                       ;; For actual WAM queries we're running, we don't want to
                       ;; LCO the final CALL because we need that stack frame
                       ;; (for storing the results).
                       :is-tail (and (not (eq clause-type :query))
                                     (null remaining)))
                   (setf first nil)))))))
    (let ((instructions (precompile-tokens wam head-tokens body-tokens))
          (variable-count (length (clause-permanent-vars clause-props))))
      ;; We need to compile facts and rules differently.  Facts end with
      ;; a PROCEED and rules are wrapped in ALOC/DEAL.
      (ecase clause-type
        (:chain
         ;; Chain rules don't need anything at all.  They just unify, set up
         ;; the next predicate's arguments, and JUMP.  By definition, in a chain
         ;; rule all variables must be temporary, so we don't need a stack frame
         ;; at all!
         nil)
        (:rule ; a full-ass rule
         ;; Non-chain rules need an ALLOC at the head and a DEALLOC right before
         ;; the tail call:
         ;;
         ;;     ALLOC n
         ;;     ...
         ;;     DEAL
         ;;     JUMP
         (circle-insert-beginning instructions `(:allocate ,variable-count))
         (circle-insert-before (circle-backward instructions) `(:deallocate)))

        (:fact
         (circle-insert-end instructions `(:proceed)))

        (:query
         ;; The book doesn't have this ALOC here, but we do it to aid in result
         ;; extraction.  Basically, to make extracting th results of a query
         ;; easier we allocate all of its variables on the stack, so we need
         ;; push a stack frame for them before we get started.  We don't DEAL
         ;; because we want the frame to be left on the stack at the end so we
         ;; can poke at it.
         (circle-insert-beginning instructions `(:allocate ,variable-count))
         (circle-insert-end instructions `(:done))))
      (values instructions clause-props))))


(defun* precompile-query ((wam wam) (query list))
  (:returns (values circle list))
  "Compile `query`, returning the instructions and permanent variables.

  `query` should be a list of goal terms.

  "
  (multiple-value-bind (instructions clause-props)
      (precompile-clause wam nil query)
    (values instructions
            (clause-permanent-vars clause-props))))


(defun* find-predicate ((clause cons))
  (:returns (values t arity))
  "Return the functor and arity of the predicate of `clause`."
  ;; ( (f ?x ?y)   | head     ||| clause
  ;;   (foo ?x)      || body  |||
  ;;   (bar ?y) )    ||       |||
  (let ((head (car clause)))
    (etypecase head
      (null (error "Clause ~S has a NIL head." clause))
      (symbol (values head 0)) ; constants are 0-arity
      (cons (values (car head) ; (f ...)
                    (1- (length head))))
      (t (error "Clause ~S has a malformed head." clause)))))


(defun* precompile-rules ((wam wam) (rules list))
  "Compile a single predicate's `rules` into a list of instructions.

  All the rules must for the same predicate.  This is not checked, for
  performance reasons.  Don't fuck it up.

  Each rule in `rules` should be a clause consisting of a head term and zero or
  more body terms.  A rule with no body is called a fact.

  Returns the circle of compiled instructions, as well as the functor and arity
  of the rules being compiled.

  "
  (assert rules () "Cannot compile an empty program.")
  (multiple-value-bind (functor arity) (find-predicate (first rules))
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



