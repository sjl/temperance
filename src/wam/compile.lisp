(in-package #:bones.wam)

;;;; Parsing
;;; Turns p(A, q(A, B)) into something like:
;;;
;;;   X0 -> p(X1, X2)
;;;   X1 -> A
;;;   X2 -> q(X1, X3)
;;;   X3 -> B

(defun parse-term (term)
  "Parse a term into a series of register assignments.

  A term is a Lispy representation of the raw Prolog.

  A register assignment is a cons of (register . assigned-to), e.g.:

    (1 . :foo)   ; X1 = Foo
    (2 . (f 1 3) ; X2 = f(X1, X3)

  "
  (labels ((variable-p (term)
             (keywordp term))
           (parse-variable (var registers)
             ;; If we've already seen this variable, just return its position,
             ;; otherwise allocate a register for it.
             (or (position var registers)
                 (vector-push-extend var registers)))
           (parse-structure (structure registers)
             (let* ((functor (first structure))
                    (arguments (rest structure))
                    (contents (list functor)))
               (prog1
                   (vector-push-extend contents registers)
                 ;; Parse the arguments and splice the results into this cell
                 ;; once we're finished.  The children should handle extending
                 ;; the registers as needed.
                 (nconc contents
                        (mapcar (lambda (arg)
                                  (parse arg registers))
                                arguments)))))
           (parse (term registers)
             (cond
               ((variable-p term)
                (parse-variable term registers))
               ;; Wrap bare symbols in a list.  Essentially: foo -> foo/0
               ((symbolp term)
                (parse (list term) registers))
               ((listp term)
                (parse-structure term registers)))))
    (let ((registers (make-array 64 :fill-pointer 0 :adjustable t)))
      (parse term registers)
      (loop :for i :from 0
            :for reg :across registers
            :collect (cons i reg)))))


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

(defun variable-assignment-p (ass)
  "Return whether the register assigment is a simple variable assignment.

  E.g. `X1 = Foo` is simple, but `X2 = f(...)` is not.

  "
  (keywordp (cdr ass)))

(defun find-dependencies (registers)
  "Return a list of dependencies amongst the given registers.

  Each entry will be a cons of `(a . b)` if register `a` depends on `b`.

  "
  (mapcan (lambda (assignment)
            (if (variable-assignment-p assignment)
              () ; Variable assignments don't depend on anything else
              (destructuring-bind (target . (functor . reqs))
                  assignment
                (declare (ignore functor))
                (loop :for req :in reqs
                      :collect (cons req target)))))
          registers))

(defun swap-cons (c)
  (cons (cdr c) (car c)))


(defun flatten (registers reverse)
  "Flatten the set of register assignments into a minimal set.

  `reverse` determines the ordering.  For queries (`nil`) we require that every
  register be assigned before it is used.  For programs (`t`) we require the
  opposite.

  We also remove the plain old variable assignments because they're not actually
  needed in the end.

  "
  (-<>> registers
    (topological-sort <>
                      (let ((dependencies (find-dependencies registers)))
                        (if reverse
                          (mapcar #'swap-cons dependencies)
                          dependencies))
                      :key #'car)
    (remove-if #'variable-assignment-p <>)))

(defun flatten-query (registers)
  (flatten registers nil))

(defun flatten-program (registers)
  (flatten registers t))


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

(defun tokenize-assignments (assignments)
  "Tokenize a flattened set of register assignments into a stream."
  (mapcan (lambda (ass)
            (destructuring-bind (register . (functor . arguments)) ass
              ;; Take a single assignment like:
              ;;   X1 = f(a, b, c)         (1 . (f a b c))
              ;;
              ;; And turn it into a stream of tokens:
              ;;   (X1 = f/3), a, b, c     (1 f 3) a b c
              (cons (list register functor (length arguments))
                    arguments)))
          assignments))


;;;; Actions
;;; Once we have a tokenized stream we can generate the list of machine
;;; instructions from it.
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

(defun generate-actions (tokens structure-inst unseen-var-inst seen-var-inst)
  "Generate a series of 'machine instructions' from a stream of tokens."
  (let ((seen (list)))
    (flet ((handle-structure (register functor arity)
             (push register seen)
             (list structure-inst functor arity register))
           (handle-register (register)
             (if (member register seen)
               (list seen-var-inst register)
               (progn
                 (push register seen)
                 (list unseen-var-inst register)))))
      (loop :for token :in tokens
            :collect (if (consp token)
                       (apply #'handle-structure token)
                       (handle-register token))))))

(defun generate-query-actions (tokens)
  (generate-actions tokens
                    #'%put-structure
                    #'%set-variable
                    #'%set-value))

(defun generate-program-actions (tokens)
  (generate-actions tokens
                    #'%get-structure
                    #'%unify-variable
                    #'%unify-value))


;;;; UI
(defun compile-query-term (term)
  "Parse a Lisp query term into a series of WAM machine instructions."
  (-> term
      parse-term
      flatten-query
      tokenize-assignments
      generate-query-actions))

(defun compile-program-term (term)
  "Parse a Lisp program term into a series of WAM machine instructions."
  (-> term
      parse-term
      flatten-program
      tokenize-assignments
      generate-program-actions))


(defun run (wam instructions &optional step)
  "Execute the machine instructions on the given WAM."
  (mapc (lambda (action)
          (when (not (wam-fail wam))
            (apply (car action) wam (cdr action))
            (when step (break))))
        instructions)
  (values))

