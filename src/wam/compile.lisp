(in-package #:bones.wam)

(defun parse-term (term)
  "Parse a term into a series of register assignments."
  ;; Turns p(A, q(A, B)) into something like:
  ;;
  ;;   X0 -> p(X1, X2)
  ;;   X1 -> A
  ;;   X2 -> q(X1, X3)
  ;;   X3 -> B
  (labels ((variable-p
             (term)
             (keywordp term))
           (parse-variable
             (var registers)
             ;; If we've already seen this variable, just return its position,
             ;; otherwise allocate a register for it.
             (or (position var registers)
                 (vector-push-extend var registers)))
           (parse-structure
             (structure registers)
             (let* ((functor (first structure))
                    (arguments (rest structure))
                    (contents (list functor)))
               (prog1
                   (vector-push-extend contents registers)
                 ;; Parse the arguments and splice the results into this cell
                 ;; once we're finished.  The children should handle extending
                 ;; the registers as needed.
                 (nconc contents
                        (mapcar #'(lambda (arg)
                                   (parse arg registers))
                                arguments)))))
           (parse (term registers)
                  (if (variable-p term)
                    (parse-variable term registers)
                    (parse-structure term registers))))
    (let ((registers (make-array 64 :fill-pointer 0 :adjustable t)))
      (parse term registers)
      (loop :for i :from 0
            :for reg :across registers
            :collect (cons i reg)))))

(defun flatten-register-assignments (registers)
  "Flatten the set of register assignments into a minimal set."
  ;; Turns:
  ;;
  ;;   X0 -> p(X1, X2)
  ;;   X1 -> A
  ;;   X2 -> q(X1, X3)
  ;;   X3 -> B
  ;;
  ;; into something like:
  ;;
  ;;   X2 -> q(X1, X3), X0 -> p(X1, X2)
  (labels ((variable-assignment-p
             (ass)
             (keywordp (cdr ass)))
           (assignment-less-p
             (ass1 ass2)
             (cond
               ;; If 2 is a variable assignment, nothing can be less than it.
               ((variable-assignment-p ass2) nil)

               ;; If 2 isn't, but 1 is, then 1 < 2.
               ((variable-assignment-p ass1) t)

               ;; Otherwise they're both structure assignments.
               ;; (N . foo A B C)      (M . bar X Y Z)
               ;;
               ;; We need to make sure that if something inside 2 uses the
               ;; target of 1, then 1 < 2.
               ((member (car ass1) (cdr ass2)) t)

               ;; Otherwise we don't care.
               (t nil))))
    (remove-if #'variable-assignment-p
               (sort registers #'assignment-less-p))))

(defun tokenize-assignments (assignments)
  "Tokenize a flattened set of register assignments into a stream."
  ;; Turns:
  ;;
  ;;   X2 -> q(X1, X3), X0 -> p(X1, X2)
  ;;
  ;; into something like:
  ;;
  ;;   (X2 = q/2), X1, X3, (X0 = p/2), X1, X2
  (mapcan #'(lambda (ass)
             (destructuring-bind (register . (functor . arguments)) ass
               ;; Take a single assignment like:
               ;;   X1 = f(a, b, c)         (1 . (f a b c))
               ;;
               ;; And turn it into a stream of tokens:
               ;;   (X1 = f/3), a, b, c     (1 f 3) a b c
               (cons (list register functor (length arguments))
                     arguments)))
          assignments))

(defun generate-actions (tokens)
  "Generate a series of 'machine instructions' from a stream of tokens."
  ;; Turns:
  ;;
  ;;   (X2 = q/2), X1, X3, (X0 = p/2), X1, X2
  ;;
  ;; into something like:
  ;;
  ;;   (#'put-structure 2 q 2)
  ;;   (#'set-variable 1)
  ;;   (#'set-variable 3)
  ;;   (#'put-structure 0 p 2)
  ;;   (#'set-value 1)
  ;;   (#'set-value 2)
  (let ((seen (list)))
    (flet ((handle-structure
             (register functor arity)
             (push register seen)
             (list #'put-structure functor arity register))
           (handle-register
             (register)
             (if (member register seen)
               (list #'set-value register)
               (progn
                 (push register seen)
                 (list #'set-variable register)))))
      (loop :for token :in tokens
            :collect (if (consp token)
                       (apply #'handle-structure token)
                       (handle-register token))))))


(defun compile-term (term)
  "Parse a Lisp term into a series of WAM machine instructions."
  (generate-actions
    (tokenize-assignments
      (flatten-register-assignments
        (parse-term term)))))

(defun run (wam instructions)
  "Execute the machine instructions on the given WAM."
  (mapc #'(lambda (action)
            (apply (car action) wam (cdr action)))
        instructions)
  (values))

