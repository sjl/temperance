(in-package #:bones.paip)

;;;; Types
(deftype logic-variable ()
  'keyword)

(deftype binding ()
  '(cons logic-variable t))

(deftype binding-list ()
  '(trivial-types:association-list keyword t))


;;;; Constants
(define-constant fail nil
  :documentation "Failure to unify")

(define-constant no-bindings '((:bones-empty-bindings . t))
  :test 'equal
  :documentation "A succesful unification, with no bindings.")

(defparameter *check-occurs* t
  "Whether to perform an occurs check.")


;;;; Unification
(defun* variable-p (term)
  (:returns boolean)
  "Return whether the given term is a logic variable."
  (keywordp term))

(defun* get-binding ((variable logic-variable)
                     (bindings binding-list))
  (:returns (or binding null))
  "Return the binding (var . val) for the given variable, or nil."
  (assoc variable bindings))

(defun* binding-variable ((binding binding))
  (:returns logic-variable)
  "Return the variable part of a binding."
  (car binding))

(defun* binding-value ((binding binding))
  "Return the value part of a binding."
  (cdr binding))

(defun* lookup ((variable logic-variable)
                (bindings binding-list))
  "Return the value the given variable is bound to."
  (binding-value (get-binding variable bindings)))

(defun* extend-bindings ((variable logic-variable)
                         (value t)
                         (bindings binding-list))
  (:returns binding-list)
  "Add a binding (var . val) to the binding list (nondestructively)."
  (cons (cons variable value)
        (if (and (eq bindings no-bindings))
          nil
          bindings)))

(defun* check-occurs ((variable logic-variable)
                      (target t)
                      (bindings binding-list))
  (:returns boolean)
  "Check whether the variable occurs somewhere in the target.

  Takes the bindings into account.  This is expensive.

  "
  (cond
   ;; If the target is this variable, then yep.
   ((eql variable target) t)

   ;; The empty list doesn't contain anything.
   ((null target) nil)

   ;; The the target is a (different) variable that has a binding, we need to
   ;; check if the variable occurs in its bindings.
   ((and (variable-p target)
         (get-binding target bindings))
    (check-occurs variable (lookup target bindings) bindings))

   ;; If the target is a list, check if any of the elements contain the variable.
   ((listp target)
    (or (check-occurs variable (first target) bindings)
        (check-occurs variable (rest target) bindings)))

   ;; Otherwise we're safe.
   (t nil)))

(defun unify (x y &optional (bindings no-bindings))
  "Unify the two terms and return bindings necessary to do so (or FAIL)."
  (flet ((unify-variable
          (variable target bindings)
          (cond
           ;; If we've already got a binding for this variable, we can try to
           ;; unify its value with the target.
           ((get-binding variable bindings)
            (unify (lookup variable bindings) target bindings))

           ;; If the target is ALSO a variable, and it has a binding, then we
           ;; can unify this variable with the target's value.
           ((and (variable-p target) (get-binding target bindings))
            (unify variable (lookup target bindings) bindings))

           ;; If this variable occurs in the target (including in something
           ;; in its bindings) and we're checking occurrence, bail.
           ((and *check-occurs* (check-occurs variable target bindings))
            fail)

           ;; Otherwise we can just bind this variable to the target.
           (t (extend-bindings variable target bindings)))))
    (cond
     ;; Pass failures through.
     ((eq bindings fail) fail)

     ;; Trying to unify two identical objects (constants or variables) can just
     ;; return the bindings as-is.
     ;;
     ;; ex: (unify :y :y) or (unify 'foo 'foo)
     ((eql x y) bindings)

     ;; Unifying a variable with something.
     ((variable-p x) (unify-variable x y bindings))
     ((variable-p y) (unify-variable y x bindings))

     ;; Unifying a non-variable with nil should fail, except for nil itself.
     ;; But that was handled with (eql x y).
     ((or (null x) (null y)) fail)

     ;; Unifying non-empty compound terms such as
     ;; (likes :x cats) with (likes sally :y).
     ((and (listp x) (listp y))
      (unify (rest x) (rest y) ; Unify the tails with the bindings gotten from...
             (unify (first x) (first y) bindings))) ; unifying the heads.

     ;; Otherwise we're looking at different constants, or a constant and a
     ;; compound term, so just give up.
     (t fail))))


;;;; Substitution
(defun* substitute-bindings ((bindings binding-list)
                             (form t))
  "Substitute (recursively) the bindings into the given form."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) form)
        ((and (variable-p form) (get-binding form bindings))
         (substitute-bindings bindings
                              (lookup form bindings)))
        ((listp form)
         (mapcar (curry #'substitute-bindings bindings) form))
        (t form)))

(defun unifier (x y)
  "Unify x with y and substitute in the bindings to get the result."
  (substitute-bindings (unify x y) x))


;;;; Database
;;; A clause is an assertion in the database.  There are two types.
;;;
;;; A fact is the "base" clause:
;;;
;;;   (likes kim cats)
;;;
;;; A rule is a way to deduce new facts from existing information:
;;;
;;;   ((likes sally :x)
;;;    (likes :x cats))
;;;
;;; Clauses are stored as lists.  The head is the first item in the list, and
;;; it's "the thing you're trying to prove".  You prove it by proving all the
;;; things in the tail of the list.  For facts the tail is empty, so they are
;;; trivially proven.
;;;
;;; A predicate is the named head of a part of a clause.  In `(likes sally :x)`
;;; the predicate is `likes`.
;;;
;;; Predicates are stored in the plists of their symbols, which is a little
;;; insane, but it's how Norvig did it so I'll do it like this for now.
(defvar *db-predicates* nil
  "A list of all the predicates in the database.")

(defconstant clause-key 'bones.paip-clauses
  "The key to use in the symbol plist for the clauses.")

(defun clause-head (clause)
  (first clause))

(defun clause-body (clause)
  (rest clause))


(defun get-clauses (pred)
  (get pred clause-key))

(defun set-clauses (pred clauses)
  (setf (get pred clause-key) clauses))


(defun predicate (relation)
  (first relation))


(defun add-clause (clause)
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred)
                 (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (set-clauses pred
                 (nconc (get-clauses pred) (list clause)))
    pred))


(defmacro rule (&rest clause)
  `(add-clause ',clause))

(defmacro fact (&rest body)
  `(add-clause '(,body)))


(defun clear-predicate (predicate)
  (setf (get predicate clause-key) nil))

(defun clear-db ()
  (mapc #'clear-predicate *db-predicates*))


(defun rename-variables (form)
  "Replace all variables in the form with new (unique) ones."
  (sublis (mapcar #'(lambda (variable) (cons variable (gensym (string var))))
                  (variables-in form))
          form))

(defun prove-all (goals bindings)
  "Returns a list of solutions to the conjunction of goals."
  ;; strap in, here we go.
  (labels ((prove-single-clause
            (goal clause bindings)
            "Try to prove a goal against a single clause using the given bindings.

            Return all possible solutions as a list of binding-lists.

            "
            ;; Try to prove a goal against a specific clause:
            ;;
            ;;     (likes sally kim)
            ;;     ((likes sally :x) (likes :x cats))
            ;;
            ;; To do this, we try to unify the goal with the head of the clause,
            ;; and then use the resulting bindings to prove the rest of the
            ;; items in the clause.
            ;;
            ;; First rename the variables in the clause, because they stand on
            ;; their own and shouldn't be confused with ones in the bindings.
            (let ((new-clause (rename-variables clause)))
              (prove-all (clause-body new-clause)
                         (unify goal (clause-head new-clause) bindings))))
           (prove
            (goal bindings)
            "Try to prove a goal, using the given bindings.

            Return all possible solutions as a list of binding-lists.

            "
            ;; We look up all the possible clauses for the goal and try proving
            ;; each individually.  Each one will give us back a list of possible
            ;; solutions.
            ;;
            ;; Then we concatenate the results to return all the possible
            ;; solutions.
            (mapcan #'prove-single-clause (get-clauses (predicate goal)))))
    (cond
     ;; If something failed further up the pipeline, bail here.
     ((eq bindings fail) fail)

     ;; If there's nothing to prove, it's vacuously true.  Return a list of the
     ;; bindings as the result.
     ((null goals) (list bindings))

     ;; Otherwise we try to prove the first thing in the list.  This gives us
     ;; back a list of possible bindings we could use.
     ;;
     ;; For each possible solution to the head, we try using it to prove the
     ;; rest of the goals and concatenate all the results.  Failed attempts are
     ;; represented as FAIL which is nil, so will collapse in the concatenation.
     (t (mapcan #'(lambda (possible-solution)
                   (prove-all (rest goals) possible-solution))
                (prove (first goals) bindings))))))

